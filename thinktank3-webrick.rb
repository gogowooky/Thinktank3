# -*- coding: utf-8 -*-
################################################################################################################
# Webrick     : routing of emacs request
# SimpleTimer : maintenance
################################################################################################################
require 'pp'
require 'webrick'
require 'benchmark'
require 'socket'
require 'json'
require 'uri'
require 'erb'
require "rbconfig"
require 'net/ftp'
require 'forwardable'

# Benchmark.bm{|r|
#  r.report( "initialize"){   @thinktank = ThinktankRoot.new( memodir: memodir ) }
# }

################################################################################################################
### 
###
################################################################################################################
def memodir_setup()
  @memodir
  @server = WEBrick::HTTPServer.new( { :BindAddress => "0.0.0.0", :DocumentRoot => Dir::pwd(), :Port => "80", :DoNotReverseLookup => true } ) # 127.0.0.1 should be avoided
  @server.mount_proc( "/" ){|req,res|
    if req.path =~ /shutdown/
      @server.shutdown
    else
      res.body            = ERB.new( IO.read( File.dirname( __FILE__ ) + "memo/memodir.html.erb" ), nil, '-' ).result( binding )
      res['Content-Type'] = 'text/html;charset=utf-8'
    end
  }
  trap( "INT" ){ @server.shutdown }
  @server.start
  @memodir
end

def app_startup ( memodir )

  ### ----------------------------------------------------------------------------------------------------------
  ### @thinktank : 初期設定
  ### ----------------------------------------------------------------------------------------------------------

  @thinktank = ThinktankRoot.new( memodir: memodir )
  @thinktank.load_config

  port     = @thinktank.configs.property( address: "config.localserver:port" )
  tempdir  = @thinktank.configs.property( address: "config.localserver:tmpdir" )
  root     = File.expand_path( File.dirname( __FILE__ ) ) + "/"
  photo    = @thinktank.configs.property( address: "config.localserver:photo" )

  @thinktank.load_memo

  ### ----------------------------------------------------------------------------------------------------------
  ### @server : 初期設定
  ### ----------------------------------------------------------------------------------------------------------

  server_setting = { :BindAddress => "0.0.0.0", :DocumentRoot => root, :Port => port, :Tempdir => tempdir, :DoNotReverseLookup => true }  # 127.0.0.1 should be avoided
  server_setting.map{|key,value| puts "WEBRICK.OPT>> %-20s| %s" % [key,value] }

  @server = WEBrick::HTTPServer.new( server_setting )

  @server.mount_proc( "/" ){|req,res| }        #### commenting it out makes root folder accessible

  @server.mount_proc( "/thinktank" ){|req,res|
    req.extend ThinktankRequest

    case req.path
    when ThinktankRequestSystem::regexp then req.extend ThinktankRequestSystem
      case req.request
      when 'restart'   then puts cmd = "ruby #{root}thinktank.rb --memodir #{memodir} --test run"; @server.shutdown; spawn( cmd )
      when 'terminate' then @server.shutdown
      end

    when ThinktankRequestStyle::regexp then req.extend ThinktankRequestStyle
      res.body            = req.prepared_content
      res['Content-Type'] = req.prepared_content_type

    when ThinktankRequestHtml::regexp then req.extend ThinktankRequestHtml
      req.report
      res.body            = ERB.new( req.prepared_content, nil, '-' ).result( binding )
      res['Content-Type'] = req.prepared_content_type

    when ThinktankRequestHtmlEdit::regexp then req.extend ThinktankRequestHtmlEdit
      req.report
      res.body            = ERB.new( req.prepared_content, nil, '-' ).result( binding )
      res['Content-Type'] = req.prepared_content_type

    when ThinktankRequestMemo::regexp then req.extend ThinktankRequestMemo
      req.report
      res.body            = ERB.new( req.prepared_content, nil, '-' ).result( binding )
      res['Content-Type'] = req.prepared_content_type

    when ThinktankRequestTopic::regexp then req.extend ThinktankRequestTopic
      req.report
      res.body            = ERB.new( req.prepared_content, nil, '-' ).result( binding )
      res['Content-Type'] = req.prepared_content_type


    end
  }

  ### ----------------------------------------------------------------------------------------------------------
  ### @server : 起動
  ### ----------------------------------------------------------------------------------------------------------
  trap( "INT" ){ @server.shutdown }
  @server.start

end




################################################################################################################
# topic用機能拡張
################################################################################################################
module ThinktankRequestTopic
  attr_accessor :date, :topic

  def self.regexp ()  Regexp.new( '\/thinktank\/(?<topic>[^\/\\\?\&]*)(\/(?<date>\d{4}\-\d{2}\-\d{2}))?\.html$', Regexp::IGNORECASE )  end
  def self.extended ( req )
    m = self.regexp.match( req.path )
    req.date        = m[:date] rescue nil
    req.topic       = m[:topic].to_s.force_encoding("utf-8").downcase rescue nil
    req.tt_erb_dir  = "#{req.tt_dir}/topic"
    req.tt_erb_file = [ req.topic, "topic" ].product([ ".#{req.tt_client}", "" ]).map{|x| "#{req.tt_erb_dir}/#{x[0]}#{x[1]}.html.erb" }.find{|x| File.exists?(x) }
    req.prepared_content      = req.layout(){  IO.read( req.tt_erb_file )  }
    req.prepared_content_type = 'text/html;charset=utf-8'
  end
  def layout()  ERB.new( IO.read( "#{tt_erb_dir}/_layout.html.erb" ), nil, '-' ).result( binding )  end
  def report ()
    super
    puts "TTREQ_TOPIC>> topic   | #{topic}"
    puts "TTREQ_TOPIC>> date    | #{date}"
  end

end

################################################################################################################
# html : memo browse用機能拡張
################################################################################################################
module ThinktankRequestHtmlBase
  attr_accessor :memoid, :format_tag, :action_tag
  def layout()  ERB.new( IO.read( "#{tt_erb_dir}/_layout.html.erb" ), nil, '-' ).result( binding )  end
  def url_to( tag, id = nil )
    super( tag, id ) || case tag
                        when :howm, :update, :delete then "#{url_to(:memos,id)}/#{id||memoid}.howm"    # http://(host):(port)/thinktank/memos/(memoid).howm 
                        when :create                 then "#{url_to(:memos)}.howm"                     # http://(host):(port)/thinktank/memos.howm
                        when :edit                   then "#{url_to(:memos)}/#{id||memoid}/edit.html"  # http://(host):(port)/thinktank/memos/(memoid)/edit.html
                        end
  end
end

module ThinktankRequestHtmlEdit    # memo requestのurl parse部
  include ThinktankRequestHtmlBase
  def self.regexp ()  Regexp.new( 'memos(\/(?<id>\d{4}\-\d{2}\-\d{2}\-\d{6}))?(\/(?<edit>edit))\.(?<ext>html)$', Regexp::IGNORECASE )  end

  def self.extended ( req )
    m = self.regexp.match( req.path )
    req.memoid       = m[:id] rescue nil
    req.format_tag   = m[:ext].to_s.downcase rescue nil
    req.action_tag   = case req.request_method
                       when "GET" then m[:id] ? :edit : :editlist
                       end rescue :error
    req.tt_erb_file = [ ".#{req.tt_client}", "" ].map{|x| "#{req.tt_erb_dir}#{req.action_tag}#{x}.#{req.format_tag}.erb" }.find{|x| File.exists?(x) }
    req.prepared_content      = req.layout(){  IO.read( req.tt_erb_file )  }
    req.prepared_content_type = 'text/html;charset=utf-8'
  end

  def report ()
    super
    puts "TTREQ_HTMLEDIT>> memoid      | #{memoid}"
    puts "TTREQ_HTMLEDIT>> action_tag  | #{action_tag}"
  end
  def url_to( tag, id = nil )
    super( tag, id ) || case tag
                        when :show   then "#{url_to(:memos,id)}/#{id||memoid}/edit.html"               # http://(host):(port)/thinktank/memos/(memoid)/edit.html
                        when :index  then "#{url_to(:memos)}/edit.html"                                # http://(host):(port)/thinktank/memos/edit.html
                        when :toggle then "#{url_to(:memos)}%s.html" % ( memoid ? "/#{memoid}" : "" )  # http://(host):(port)/thinktank/memos/(memoid).html
                        end
  end
end


module ThinktankRequestHtml    # memo requestのurl parse部
  include ThinktankRequestHtmlBase
  def self.regexp ()  Regexp.new( 'memos(\/(?<id>\d{4}\-\d{2}\-\d{2}\-\d{6}))?\.(?<ext>html)$', Regexp::IGNORECASE )  end
  def self.extended ( req )
    m = self.regexp.match( req.path )
    req.memoid       = m[:id] rescue nil
    req.format_tag   = m[:ext].to_s.downcase rescue nil
    req.action_tag   = case req.request_method
                       when "GET" then m[:id] ? :show : :index
                       end rescue :error
    req.tt_erb_file = [ ".#{req.tt_client}", "" ].map{|x| "#{req.tt_erb_dir}#{req.action_tag}#{x}.#{req.format_tag}.erb" }.find{|x| File.exists?(x) }
    req.prepared_content      = req.layout(){  IO.read( req.tt_erb_file )  }
    req.prepared_content_type = 'text/html;charset=utf-8'
  end

  def report ()
    super
    puts "TTREQ_HTML>> memoid      | #{memoid}"
    puts "TTREQ_HTML>> action_tag  | #{action_tag}"
  end

  def url_to( tag, id = nil )
    super( tag, id ) || case tag
                        when :show   then "#{url_to(:memos,id)}/#{id||memoid}.html"                         # http://(host):(port)/thinktank/memos/(memoid).html 
                        when :index  then "#{url_to(:memos)}.html"                                          # http://(host):(port)/thinktank/memos.html
                        when :toggle then "#{url_to(:memos)}%s/edit.html" % ( memoid ? "/#{memoid}" : "" )  # http://(host):(port)/thinktank/memos/(memoid)/edit.html
                        end
  end
end

################################################################################################################
# memo : howm file用機能拡張
################################################################################################################
module ThinktankRequestMemo    # memo requestのurl parse部
  attr_accessor :memoid, :format_tag, :action_tag
  def self.regexp ()  Regexp.new( 'memos(\/(?<id>\d{4}\-\d{2}\-\d{2}\-\d{6}))?\.(?<ext>howm)$', Regexp::IGNORECASE )  end
  def self.extended ( req )
    m = self.regexp.match( req.path )
    req.memoid      = m[:id] rescue nil
    req.format_tag  = m[:ext].to_s.downcase rescue nil
    req.action_tag  = case req.request_method
                      when 'GET'     then m[:id] ? :show   : :index
                      when 'POST'    then m[:id] ? :error  : :create
                      when 'PUT'     then m[:id] ? :update : :error
                      when 'DELETE'  then m[:id] ? :delete : :error
                      end rescue :error
    req.tt_erb_file = [ ".#{req.tt_client}", "" ].map{|x| "#{req.tt_erb_dir}#{req.action_tag}#{x}.#{req.format_tag}.erb" }.find{|x| File.exists?(x) }
    req.prepared_content      = IO.read( req.tt_erb_file )
    req.prepared_content_type = 'text/plain;charset=utf-8'
  end

  def report ()
    super
    puts "TTREQ_MEMO>> memoid      | #{memoid}"
    puts "TTREQ_MEMO>> action_tag  | #{action_tag}"
  end
end


################################################################################################################
# style : CSS,JS,js-package用機能拡張
################################################################################################################
module ThinktankRequestStyle
  
  def self.regexp ()  Regexp.new( '(style|ace|bootstrap)(\/.*(\.([^\.]+)))$', Regexp::IGNORECASE )  end
  def self.extended ( req ) 
    ext_ct_hash = { css: 'text/css;charset=utf-8', js: 'text/javascript;charset=utf-8' }
    m = self.regexp.match( req.path )
    req.prepared_content      = IO.read( "#{req.tt_dir}/#{m[1]}#{m[2]}" )
    req.prepared_content_type = ext_ct_hash[ m[4].downcase.to_sym ]
  end

end

################################################################################################################
# restart : Webrick restart用機能拡張
################################################################################################################
module ThinktankRequestSystem
  attr_accessor :request
  def self.regexp ()  Regexp.new( '\/(restart|terminate)$', Regexp::IGNORECASE )  end
  def self.extended ( req )
    m = self.regexp.match( req.path )
    req.request = m[1]
  end
end


################################################################################################################
### http request : 機能拡張
################################################################################################################
module ThinktankRequest     # http requestの基礎部分
  attr_accessor :tt_client, :tt_url, :tt_dir, :tt_erb_dir, :tt_erb_file
  attr_accessor :prepared_content, :prepared_content_type

  def self.agent_regexp ()  Regexp.new( '(emacs|chrome|firefox|safari)', Regexp::IGNORECASE )  end
  def self.tturl_regexp ()  Regexp.new( '^(.*\/thinktank)\/.*', Regexp::IGNORECASE )  end
  def self.extended ( req ) 
    req.tt_client  = ( self.agent_regexp =~ req['User-Agent'] ? $1.downcase : nil )
    req.tt_url     = ( self.tturl_regexp =~ "#{req.host}:#{req.port}#{req.unparsed_uri}" ? $1 : nil )  # (host):(port)/thinktank
    req.tt_dir     = File.expand_path( File.dirname( __FILE__ ) )
    req.tt_erb_dir = "#{req.tt_dir}/memos/"
    req.prepared_content      = 'not prepared'
    req.prepared_content_type = 'unknown'
  end

  def url_to( tag, id = nil )
    case tag
    when :thinktank, :base then "http://#{tt_url}"          # http://(host):(port)/thinktank
    when :memos            then "http://#{tt_url}/memos"    # http://(host):(port)/thinktank/memos 
    end
  end

  def report ()
    puts "TTREQ>> ========================================================================="
    puts "TTREQ>> path                  | #{path}"
    puts "TTREQ>> method                | #{request_method}"
    puts "TTREQ>> options               | #{Hash[query.map{|k,v|[k,v.force_encoding("UTF-8")]}].to_s}"
    puts "TTREQ>> req_url               | #{request_uri}"
    puts "TTREQ>> prepared_content_type | #{prepared_content_type}"
    puts "TTREQ>> prepared_content      | #{prepared_content[0..80]}"
    puts "TTREQ>> tt_client             | #{tt_client}"
    puts "TTREQ>> tt_url                | #{tt_url}"
    puts "TTREQ>> tt_dir                | #{tt_dir}"
    puts "TTREQ>> tt_erb_dir            | #{tt_erb_dir}"
    puts "TTREQ>> tt_erb_file           | #{tt_erb_file}"
  end

end


################################################################################################################
# http server : 機能拡張
################################################################################################################
class ThinktankServer < WEBrick::HTTPServer
end

module WEBrick
  module HTTPServlet
    class ProcHandler < AbstractServlet
      alias :do_PUT    :do_GET
      alias :do_DELETE :do_GET
    end
  end
end


################################################################################################################
# file server : 機能拡張
################################################################################################################
class ThinktankFileServer
  attr_accessor :size

  def initialize( user: '', pswd: '', host: '', basepath: '', port: '21', tempdir: '' )
    @user, @pswd, @host, @basepath, @port, @tempdir = user, pswd, host, basepath, port, tempdir
    if 0 < host.size then
      @ftp = Net::FTP.new
      @ftp.connect( @host, @port )
      @ftp.login( @user, @pswd )
      @ftp.passive = true
    end
  end
  
  def getfile ( filepath )
    if filepath.match( /.*([^\/]+)$/ ) then
      if @ftp then
        @ftp.getbinaryfile( "#{@basepath}/#{filepath}", "#{@tempdir}/#{$1}" )
        @size = File::Stat.new( "#{@tempdir}/#{$1}" ).size
        open( "#{@tempdir}/#{$1}" ){|file| return file.read }
      end
    end
  end

  def chdir ( path ) @ftp.chdir( "#{@basepath}/#{path}" )  end
  def ls ()   @ftp.ls  end
  def quit () @ftp.quit if @ftp  end
end









=begin
################################################################################################################
# http request : photo用機能拡張
################################################################################################################

      #res.body            = req.prepared_content
      #res.content_length  = req.prepared_content_size
      #res['Content-Type'] = req.prepared_content_type

module ThinktankRequestPhoto    # photo requestのurl parse部
  attr_accessor :photoname, :photopath
  attr_accessor :action_tag, :erb_file

  def self.regexp ()  Regexp.new( 'photos\/(\d{4}\-\d{2}\-\d{2})/(.+)\.(png|gif|jpg)$', Regexp::IGNORECASE )  end
  def self.ftp()      Regexp.new( '^ftp:\/\/((\w+):(\w+)@)?([^\/]+)(/.*)$', Regexp::IGNORECASE )  end
  def self.imageF()   Regexp.new( '^(((\d{4})\-\d{2}\-\d{2})\-\d{6}\-\d{3})$', Regexp::IGNORECASE )  end

  def self.extended ( req )
    case req.path
    when self.regexp
      req.action_tag = case $2
                       when Regexp.new( '^(((\d{4})\-\d{2}\-\d{2})\-\d{6}\-\d{3})$', Regexp::IGNORECASE )   # 年月日-番号,          /photos/2010-01-01/010101-001.ext  | html, binary
                         req.photopath, req.photoname = "#$3/#$2/", $1[2..-1]
                         :photoid
                       else
                         req.photopath, req.photoname = "#$2/#$1/", $3
                         :photoid
                       end
    end
    
    req.erb_file = [ ".#{req.client_app}", "" ].map{|x| "#{req.tt_dir}/photos/#{req.action_tag}#{x}.#{req.format_tag}.erb" }.find{|x| File.exists?(x) }

  end

  def report ()
    super
    puts "TT_REQ_PHOTO>> photopath  | #{photopath}"
    puts "TT_REQ_PHOTO>> photoname  | #{photoname}.#{format_tag}"
    puts "TT_REQ_PHOTO>> action_tag | #{action_tag}"
    puts "TT_REQ_PHOTO>> erb_file   | #{erb_file}"
  end

end

################################################################################################################
# http request : photo用機能拡張
################################################################################################################
module ThinktankRequestPhotos    # photo requestのurl parse部
  attr_accessor :photoname, :photopath
  attr_accessor :action_tag, :erb_file

  def ThinktankRequestPhoto.regexp ()  Regexp.new( 'photos(\/.*)?\.(png|gif|jpg|html)$',     Regexp::IGNORECASE )  end
  def ThinktankRequestPhoto.ftp()      Regexp.new( '^ftp:\/\/((\w+):(\w+)@)?([^\/]+)(/.*)$', Regexp::IGNORECASE )  end

  def self.extended ( req )
    case req.path
    when Regexp.new( 'photos(\/(.*))?\.(png|gif|jpg|html)$', Regexp::IGNORECASE )
      req.action_tag = case $2
                       when Regexp.new( '^((\d{4})\-\d{2}\-\d{2})\/(.*)$', Regexp::IGNORECASE )             # 年月日/ファイル名,    /photos/2010-01-01/ファイル名.html        | html, binary
                         req.photopath, req.photoname = "#$2/#$1/", $3
                         :photoid
                       when Regexp.new( '^(((\d{4})\-\d{2}\-\d{2})\-\d{6}\-\d{3})$', Regexp::IGNORECASE )   # 年月日-番号,          /photos/2010-01-01-010101-001.html        | html, binary
                         req.photopath, req.photoname = "#$3/#$2/", $1[2..-1]
                         :photoid
                       when Regexp.new( '^((\d{4})\-\d{2}\-\d{2})$', Regexp::IGNORECASE )                   # 年月日,               /photos/2010-01-01.htm                    | html
                         req.photopath, req.photoname = "#$2/#$1/", nil
                         :day
                       when Regexp.new( '^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)$', Regexp::IGNORECASE ) # 月,           /photos/Jan.html&limit=100                | html
                         :monthall
                       when Regexp.new( '^(\d{2})$', Regexp::IGNORECASE )                                           # 月,           /photos/01.html&limit=100                 | html
                         :monthall
                       when Regexp.new( '^(\d{4})$', Regexp::IGNORECASE )                                   # 年(制限),             /photos/2010.html&limit=100               | html
                         req.photopath, req.photoname = $1, nil
                         :yearall
                       when Regexp.new( '^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\d{2}$', Regexp::IGNORECASE ) # 月日,    /photos/Jan01.html&limit=100              | html
                         :monthday
                       else                                                                                              # 全て,    /photos.html&limit=100                    | html
                         :all
                       end
    end
    
    req.erb_file = [ ".#{req.client_app}", "" ].map{|x| "#{req.tt_dir}/photos/#{req.action_tag}#{x}.#{req.format_tag}.erb" }.find{|x| File.exists?(x) }

  end

  def report ()
    super
    puts "TT_REQ_PHOTO>> photopath  | #{photopath}"
    puts "TT_REQ_PHOTO>> photoname  | #{photoname}.#{format_tag}"
    puts "TT_REQ_PHOTO>> action_tag | #{action_tag}"
    puts "TT_REQ_PHOTO>> erb_file   | #{erb_file}"
  end

end
=end



=begin
    when ThinktankRequestPhoto::regexp then req.extend ThinktankRequestPhoto
      req.report
      #res.body            = req.prepared_content
      #res.content_length  = req.prepared_content_size
      #res['Content-Type'] = req.prepared_content_type

    when ThinktankRequestPhoto::regexp then req.extend ThinktankRequestPhoto
      req.report
      fs = case photo
           when ThinktankRequestPhoto2::ftp
             ThinktankFileServer.new( user: $2, pswd: $3, host: $4, basepath: $5, tempdir: tempdir )  # ftp://user:pswd@host:port/basepath/
           end

      case req.format_tag
      when 'png', 'gif', 'jpg'
        res.body = fs.getfile( "#{req.photopath}#{req.photoname}.#{req.format_tag.upcase()}" )
        res.content_length = fs.size
        res['Content-Type'] = req.content_type
      else
        fs.chdir( req.photopath )
        res.body = fs.ls.join("<BR>\n")
        # res.body  = ERB.new( IO.read( req.erb_file ), nil, '-' ).result( binding ) rescue "routing error"
        res['Content-Type'] = req.content_type
      end
      fs.quit
=end      
