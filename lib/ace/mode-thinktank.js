define("ace/mode/thinktank_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"], function(require, exports, module) {
		"use strict";

		var oop = require("../lib/oop");
		var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;
		
		var ThinktankHighlightRules = function() {
				this.$rules = 
						{
								"start": [
										{   
												token : "entity.name.section",
												regex : "^\\*+[ 　\t]+.*$"
										},{ 
												token: "constant.underline.link",  
												regex: "[0-9\\-]{17}\\.howm"
										},{                                　　// :PROPERTIES: ... :END:
												token: "comment",
												regex: "^[ 　\t]*:(PROPERTIES|DESCRIPTION|END):"
										},{　　　　　　　　　　　　　　　　　　// #+BEGIN_ ... :#+END_
												token: "comment",
												regex: "^[ 　\t]*#\\+(BEGIN|END).*$"
										},{  　　　　　　　　　　　　　　　　　// [[ link ][ title ]]
												token: ["constant.underline.link", "keyword.operator"], 
												regex: "\\[\\[([^\\[\\]]+)\\](\\[[^\\[\\]]+\\])?\\]"
										},{  　　　　　　　　　　　　　　　　　// [ date ] or < date >
												token: "markup.italic.link", 
												regex: "(\\[|\\<)[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}.*(\\]|\\>)"
										},{
												token : "constant.underline.link", // http
												regex : "https?://[^ 　\t]+"
										},{
												token : "string",
												regex : '"(?=.)',
												next  : "qqstring"
										}
								],
								"qqstring": [
										{
												token: "constant.character.escape.lisp",
												regex: "\\\\."
										},{
												token : "string",
												regex : '[^"\\\\]+'
										},{
												token : "string",
												regex : "\\\\$",
												next  : "qqstring"
										},{
												token : "string",
												regex : '"|$',
												next  : "start"
										}
								]
						}
				
		};
		
		oop.inherits(ThinktankHighlightRules, TextHighlightRules);
		
		exports.ThinktankHighlightRules = ThinktankHighlightRules;
});



define("ace/mode/thinktank",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/thinktank_highlight_rules"], function(require, exports, module) {
		"use strict";
		
		var oop = require("../lib/oop");
		var TextMode = require("./text").Mode;
		var ThinktankHighlightRules = require("./thinktank_highlight_rules").ThinktankHighlightRules;
		
		var Mode = function() {
				this.HighlightRules = ThinktankHighlightRules;
		};
		oop.inherits(Mode, TextMode);
		
		(function() {
				this.lineCommentStart = ";";
				this.$id = "ace/mode/thinktank";
		}).call(Mode.prototype);
		
		exports.Mode = Mode;
});


