# -*- coding: utf-8 -*-
################################################################################################################
# Thinktankメモ管理
################################################################################################################
require 'benchmark'
require 'socket'
require 'minitest/autorun'
require 'minitest/unit'

load 'thinktank3-webrick.rb'
load 'thinktank3-object.rb'

#
# 起動方法
#
# ruby thinktank.rb --memodir "D:/Dropbox/MyData/tt/" --test test
# ruby thinktank.rb --memodir ~/Dropbox/MyData/tt
#


# 初期値設定
$memodir, $test = nil, "thinktank"


# コマンドラインオプション読み込み
eval( "$#{$1} = ARGV.shift" ) while ARGV.shift =~ /^\-\-(\w+)/

puts "CMDLINE.OPT>> test     | #{$test}"
puts "CMDLINE.OPT>> memodir  | #{$memodir}"
puts "CMDLINE.ENV>> hostname | #{Socket.gethostname}"

case $test

when "run"
  app_startup( $memodir )

when "ttbm"
  Benchmark.bm 10 do |r|
    r.report( "initialize" ){ @tt2 = ThinktankRoot.new( memodir: $memodir ) }
    r.report( "configure"  ){ @tt2.configure }
    r.report( "load"       ){ @tt2.load }
    r.report( "configs"    ){ @tt2.configs["0000-00-00-000001"][/Thinktank$/][/Template/].caption }
    r.report( "memos"      ){ @tt2.memos["0000-00-00-000001"][/Thinktank$/][/Template/].caption }
  end


when "thinktank"
  class TestThinktank < MiniTest::Test
    def setup
      @tt = ThinktankRoot.new( memodir: $memodir )
      @tt.load_config
    end

    def test_thinktank_memos
      @tt.load_memo
      assert_equal( @tt.memos.size, 4924 )
      assert_equal( @tt.memos["0000-00-00-000000"].caption, "HOHM top menu ver.00.003.0" )
      assert_equal( @tt.memos["0000-00-00-000001"].caption, "Thinktank Registory ver.00.002.0" )
      assert_equal( @tt.memos["0000-00-00-000002"].caption, "開発用のメモデータ" )
      assert_equal( @tt.memos["0000-00-00-000001"][/Directories/].caption, "Directories" )
      assert_equal( @tt.memos["0000-00-00-000001"][/Thinktank$/][/Host/].subsections.size, 13 )
      assert_equal( @tt.memos["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].properties["host"], "http://gogowooky.mycloudnas.com" )

      assert_equal( @tt.memos.show( memoid: "0000-00-00-000001" ), @tt.memos["0000-00-00-000001"] )
      assert_equal( @tt.memos.edit( memoid: "0000-00-00-000001" ), @tt.memos["0000-00-00-000001"] )

    end

    def test_thinktank_initialize
      assert_equal( @tt.memodir, $memodir ) 
    end

    def test_thinktank_configs
      assert_equal( @tt.configs.size, 4 )
      assert_equal( @tt.configs["0000-00-00-000000"].caption, "HOHM top menu ver.00.003.0" )
      assert_equal( @tt.configs["0000-00-00-000001"].caption, "Thinktank Registory ver.00.002.0" )
      assert_equal( @tt.configs["0000-00-00-000002"].caption, "開発用のメモデータ" )
    end

    def test_thinktank_configs_topsection
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/].caption, "Thinktank" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/].subsections.size, 3 )
      assert_equal( @tt.configs["0000-00-00-000001"][/Directories/].caption, "Directories" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Directories/].subsections.size, 6 )
    end

    def test_thinktank_configs_subsection
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Template/].caption, "Template" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Template/].subsections.size, 0 )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/].caption, "Host" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/].subsections.size, 13 )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].caption, "gogowooky" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].subsections.size, 0 )
    end

    def test_thinktank_configs_subsection_properties
      # puts @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].content
      # puts @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].properties
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].properties["host"], "http://gogowooky.mycloudnas.com" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].properties["port"], "20080" )
      assert_equal( @tt.configs["0000-00-00-000001"][/Thinktank$/][/Host/][/gogowooky/].properties["root"], "/thinktank/" )
      assert_equal( @tt.configs["0000-00-00-000001"].property( address: "Thinktank.Host.gogowooky:root"), "/thinktank/" )
      assert_equal( @tt.configs.property( address: "Thinktank.Host.gogowooky:root"), "/thinktank/" )
      assert_equal( @tt.configs["0000-00-00-000001"].property( address: "Thinktank.Host.thinktank:imagedir"), "smb://QNAP/Multimedia/photo/" )
      assert_equal( @tt.configs.property( address: "Thinktank.Host.thinktank:imagedir"), "smb://QNAP/Multimedia/photo/" )
      assert_equal( @tt.configs.property( address: "config.localserver:port"), "20091" )
    end

    def test_thinktank_configs_subsection_properties_2
      assert_equal( @tt.configs.property( address: "config.localserver:tmpdir"), "/Users/gogowooky/temp/" )
    end

  end

end



