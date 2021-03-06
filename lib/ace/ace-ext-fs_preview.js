// --
// FreeSpace Preview for ACE
// questions? akumpf@gmail.com
// --
// NOTE: This is an experimental extension. If you have suggestions, please share!
// --
// This extension adds a preview layer to the ace rendering stack to show
// embedded visual information in the free space below a link (if available).
// Links are assumed to be anything that the tokenizer labels as "link".
// --
// To enable FreeSpacePreviews, include this file and set the ACE editor's options:
//
//    editor.setOptions({enableFreeSpacePreviews: true});
//
// --

define("ace/ext/fs_previews", ["require","exports","module","ace/editor","ace/config"], function(require, exports, module) {

var fsPreviewCss = "\
.ace_fs_preview {\
position: absolute;\
right: 20px;\
border-left: 2px dotted rgba(128,128,128,0.5);\
padding: 2px;\
padding-left: 7px;\
overflow: hidden;\
cursor: text;\
}\
.ace_fs_preview > * {pointer-events: auto;}\
\
.ace_fs_preview img {height: 100%; width: auto; background-color: rgba(128,128,128,0.85);}\
.ace_fs_preview img:hover {outline: 2px solid #808080;}\
\
.ace_fs_preview iframe {height: 100%; width: 100%; background: #808080;}\
";

var dom = require("../lib/dom");
dom.importCssString(fsPreviewCss, "ace_fs_previews");

var Editor = require("ace/editor").Editor;

var MAX_UNSEEN = 25; // don't immediately remove unseen previews. keep the last few around.

function stringHashAbs(str){
	str = str||"";
  var hash = 0, i, chr, len;
  if (str.length == 0) return hash;
  for (i = 0, len = str.length; i < len; i++) {
    chr   = str.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash<0?-hash:hash;
}

function onAfterRender(err,renderer){
	//console.log("afterRender", renderer);
	// $renderSimpleLine, $renderWrappedLine
	// --
	var $previews = $(renderer.container).find(".ace_content .ace_layer.ace_fs_previews");
	//if(!$previews) return;
	$previews.find(".ace_fs_preview").addClass("unseen");
	// --
	$(renderer.content).find(".ace_line .ace_link").each(function(index, el){
		var $el = $(el);
		var url = $el.text();
		// --
		var mtype = null;
		var mres  = null;
		// --
		if(!mtype){
			mres  = url.match(/(?:http:\/\/)?(?:www\.)?(?:youtube\.com|youtu\.be)\/(?:watch\?v=)?([^<]+)/);
			if(mres) mtype = "youtube";
		}
		if(!mtype){
			mres  = url.match(/.*\.(jpg|gif|png|jpeg|ico|svg|bmp)$/);
			if(mres) mtype = "image";
		}
		// --
		if(mtype){
			var $lg = $el.parents(".ace_line_group");
			var pid  = "fsp_id_"+stringHashAbs($lg.text())+"_"+stringHashAbs(url);
			//console.log($lg, $el);
			var blanklines = 0;
			var blankH     = 0;
			var nexts = $lg.nextAll(".ace_line_group");
			for(var i=0; i<nexts.length; i++){
				var $next = $(nexts[i]);
				if($.trim($next.text()) != "") break;
				blankH += $next.height();
				blanklines++;
			}
			//console.log("URL + Blanklines", url, blanklines, blankH+"px");
			if(blanklines > 1){
				var ptop  	= ($el.position().top+$el.height()+2)+"px";
				var pleft 	= ($el.position().left+6)+"px";
				var pheight = Math.min(320, blankH-8)+"px";
				var pwidth  = "auto";
				var $pel 		= $previews.find("#"+pid);
				// --
				var content = "...";
				switch(mtype){
					case "youtube":
						content = '<iframe src="http://www.youtube.com/embed/'+mres[1]+
							'?modestbranding=1&rel=0&wmode=transparent&theme=light&color=white"\
							 frameborder="0" allowfullscreen></iframe>';
						pwidth = Math.max(120, Math.min(640, Math.ceil(parseFloat(pheight)*16.0/9.0))) + "px";
						//console.log("Pwidth: ", pwidth);
						break;
					case "image":
						content = "<a href='"+url+"' target='_blank'><img src='"+url+"' /></a>";
						break;
				}
				// --
				if($pel.length == 0){
					$previews.prepend("<div class='ace_fs_preview' id='"+pid+"' style='top: "+ptop+"; left: "+pleft+"; height: "+pheight+"; width: "+pwidth+";'>"+content+"</div>");
				}else{
					$pel.css({top: ptop, left: pleft, height: pheight, width: pwidth}).removeClass("unseen").show();
				}
			}
		}
	});
	// --
	$unseen = $previews.find(".ace_fs_preview.unseen");
	$unseen.hide();
	if($unseen.length > MAX_UNSEEN){
		//console.log("Removing extra unseen previews:", $unseen.length-MAX_UNSEEN);
		$unseen.slice(-($unseen.length-MAX_UNSEEN)).remove();
	}
}

require("../config").defineOptions(Editor.prototype, "editor", {
  enableFreeSpacePreviews: {
    set: function(val) {
      if (val) {
				console.log("FreeSpacePreviews: Enabled");
				this.renderer.on("afterRender",onAfterRender);
				$(this.container).find(".ace_content").append("<div class='ace_layer ace_fs_previews'></div>");
      } else {
				console.log("FreeSpacePreviews: Disabled");
				this.renderer.off("afterRender",onAfterRender);
				$(this.container).find(".ace_content .ace_layer.ace_fs_previews").remove();
      }
    },
    value: false
  }
});
});

(function() {
    window.require(["ace/ext/fs_previews"], function() {});
})();

