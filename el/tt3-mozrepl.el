;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt-mozrepl.el  thinktank

;; repl._workContext → Windowオブジェクト
(require 'moz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public 関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun thinktank3-mozrepl-remote () (interactive)
	(tt3-mozrepl-open-firefox)

	(unwind-protect 
			(catch 'exit-menu
				(let (input-key prev-command)
					(while t
						(setq input-key (read-key (format "commnand: %s" prev-command))) ;; (read-key)
						(setq prev-command
									(case input-key
										(7   (throw 'exit-menu 1) "quit") ;; C-g
										(27  (throw 'exit-menu 2) "quit") ;: ESC
										((16 up)     (tt3-mozrepl-request  "goDoCommand('cmd_scrollLineUp');") "up") ;; C-p
										((14 down)   (tt3-mozrepl-request  "goDoCommand('cmd_scrollLineDown');") "down") ;; C-n
										((2 left)    (tt3-mozrepl-request  "gBrowser.tabContainer.advanceSelectedTab(-1, true);") "left")  ;; C-b
										((6 right)   (tt3-mozrepl-request  "gBrowser.tabContainer.advanceSelectedTab(1, true);")  "right") ;; C-f
										
										((8 127)     (tt3-mozrepl-request  "gBrowser.goBack();")    "back")      ;; C-h, BS
										(S-backspace (tt3-mozrepl-request  "gBrowser.goForward();") "forward")   ;; S-BS
										
										(23      (tt3-mozrepl-request      "gBrowser.removeCurrentTab();") "close tab") ;; C-w
										(18      (kill-buffer "*MozRepl*") "wfreset mozrepl") ;; C-r
										(C-left  (tt3-mozrepl-request      "gBrowser.moveTabBackward();") "tab left")   ;; C-left
										(C-right (tt3-mozrepl-request      "gBrowser.moveTabForward();") "tab right")   ;; C-right
										(t "nop")
										))
						))))) ; (tt3-mozrepl-request  "goDoCommand('search');")



(defadvice browse-url (around tt3-mozrepl-browse-url-with-firefox activate )
	(condition-case nil
			(progn (tt3-mozrepl-open-firefox)
						 (tt3-mozrepl-request (format "gBrowser.selectedTab = gBrowser.loadOneTab('%s'); " (ad-get-arg 0) )))
		(error ad-do-it)))

'((ad-disable-advice 'browse-url 'around 'tt3-mozrepl-browse-url-with-firefox))


(defun tt:mozrepl-insert-current-page-node () (interactive)
	(insert (format-time-string "\n* [%Y-%m-%d %a] "))
	(tt:mozrepl-insert-current-page-link))


;;
;; linkを挿入、選択文字あればそれにurlをattach。　ここを参照、とかやるとき便利。
;;
(defun tt:mozrepl-insert-current-page-link () (interactive)
	(insert (let ((title (if (use-region-p)
													 (prog1 (buffer-substring (region-beginning) (region-end))
														 (kill-region (region-beginning) (region-end)))
												 (thinktank3-mozrepl-get-title))))
						(format "[[%s][%s]]"
										(thinktank3-mozrepl-get-url)
										(replace-regexp-in-string "[\]\[]" "|" title)))))

(defun thinktank3-mozrepl-get-current-page-link () 
	(format "[[%s][%s]]" (thinktank3-mozrepl-get-url) (replace-regexp-in-string "[\]\[]" "|" (thinktank3-mozrepl-get-title))))


'((thinktank3-mozrepl-insert-current-page))
'((tt3-mozrepl-request "repl._workContext.sidebar; "))
'((tt3-mozrepl-request "/* scontent.location.href */ true;"))



(defun tt:mozrepl-memo-docx ( &optional memoid ) (interactive)
	(tt3-mozrepl-request (format "content.location.href = 'http://localhost:20080/thinktank/memos/%s.docx'; " 
															 (thinktank3-format :memoid (or memoid (thinktank3-menu-buffer-on-useraction))))))

(defun tt:mozrepl-browse-memo ( &optional memoid ) (interactive)
	
	(let* ((url (format "http://localhost:20080/thinktank/memos/%s.html" (thinktank3-format :memoid (or memoid (thinktank3-menu-buffer-on-useraction)))))
				 sections nest secnum)
		(save-excursion
			(when (re-search-backward "^\\(\\*+\\)" nil t)
				(setq nest (length (match-string 1)))
				(setq secnum 1)
				(while (< 0 nest)
					(while (and (re-search-backward (format "^\\(\\*+\\)" nest) nil t) (<= nest (length (match-string 1))))
						(when (= nest (length (match-string 1))) (setq secnum (+ 1 secnum))))
					(push secnum sections)
					(setq nest (- nest 1))
					(setq secnum 1)))
			(browse-url (format "%s#%s" url (mapconcat 'number-to-string sections "."))))))



(defun thinktank3-mozrepl-get-url () (interactive) (tt3-mozrepl-request "repl._workContext.content.location.href; "))
(defun thinktank3-mozrepl-get-title () (interactive) (tt3-mozrepl-request "repl._workContext.content.document.title; "))


;;
;; るりま
;; 
(defun tt:mozrepl-rurema-search () (interactive)
	(tt3-mozrepl-browse-url "http://rurema.clear-code.com/version:2.0.0/")
	(sleep-for 0.5)
	(tt3-mozrepl-request    (format "gBrowser.contentDocument.getElementById('query-input').value = '%s';" (focused-string)))
	(tt3-mozrepl-request    "gBrowser.contentDocument.forms[0].submit()" ))
;; (tt:mozrepl-rurema-search)

;;
;; Ruby Reference
;;
(defun tt:mozrepl-rubyref-search () (interactive)
	(tt3-mozrepl-browse-url "http://miyamae.github.io/rubydoc-ja/2.0.0/#!/doc/index.html")
	(tt3-mozrepl-request    (format "gBrowser.contentDocument.getElementById('search-box').value = '%s';" (focused-string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local 関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn ;; emacsで用いるdefault-browserをfirefoxにする
	(setq browse-url-browser-function 'browse-url-generic)
	(when (thinktank3-config :firefox) (setq browse-url-generic-program (thinktank3-config :firefox)))
	)

'((tt3-mozrepl-browse-url (format "http://lsd-project.jp/weblsd/begin/%s" (url-hexify-string "特許"))))

(defun tt3-mozrepl-browse-url ( url )
	(tt3-mozrepl-open-firefox)
	(condition-case nil
			(tt3-mozrepl-request (format "content.location.href = '%s'; " url ))
		(error (browse-url url))))

(defun tt3-mozrepl-request ( attr )
	(when (tt3-mozrepl-open-firefox)
		(set-process-coding-system (inferior-moz-process) 'utf-8-unix)
		(comint-send-string (inferior-moz-process) attr)
		(sleep-for 0 150)

		(save-excursion
			(set-buffer (process-buffer (inferior-moz-process)))
			(goto-char (point-max))
			(previous-line)
			(buffer-substring-no-properties
			 (+ (point-at-bol) (length moz-repl-name) 3)
			 (- (point-at-eol) 1)))))

(defun tt3-mozrepl-req-jscode ( &rest jscode ) (tt3-mozrepl-open-firefox) (tt3-mozrepl-request (mapconcat 'identity jscode "")))

(defun tt3-mozrepl-open-firefox ()
	(unless (find-if (lambda (x) (string-match "\\(MozRepl\\|firefox\\)" (process-name x))) (process-list))
		(start-process-shell-command "firefox" nil (thinktank3-config :firefox))
		(sleep-for 2))
	(inferior-moz-process))

'((tt3-mozrepl-open-firefox))
'((start-process-shell-command "firefox" nil (thinktank3-config :firefox)))

;; -----------------------------------------------------------------------------------------------------------------------------------

'((progn	
	(tt3-mozrepl-browse-url "http://miyamae.github.io/rubydoc-ja/2.0.0/#!/doc/index.html")
	(tt3-mozrepl-request    "gBrowser.contentDocument.getElementById('search-box').value = 'File exist';" )
	(tt3-mozrepl-browse-url "http://rurema.clear-code.com/version:2.0.0/")
	(tt3-mozrepl-request    "gBrowser.contentDocument.getElementById('query-input').value = 'File exist';" )
	(tt3-mozrepl-request    "gBrowser.contentDocument.forms[0].submit()" )

	(condition-case nil (tt3-mozrepl-request "window.alert( \"egashira\" ); ") (error nil))
	
	(tt3-mozrepl-request "window.alert( \"egashira\" ); ") ;; alert dialog boxも出せる。
 	 (tt3-mozrepl-request "window.prompt( \"egashira\" ); ") ;; 値も取り出せているが、tt3-mozrepl-request の出来が良くない。
	 (tt3-mozrepl-nonread)
	 (progn
		 (sit-for 3)
		 (tt3-mozrepl-request "window.getSelection().getRangeAt(0).toString(); ") ) ;; getSelection()はうまく動かない。

	 (progn
 		 (tt3-mozrepl-request (concat "emacs_tab = gBrowser.loadOneTab('http://google.com'); "
																	"gBrowser.selectedTab = emacs_tab;" ))
 		 (tt3-mozrepl-request (concat "gBrowser.selectedTab = emacs_tab;" )) ;; active可

 		 (tt3-mozrepl-request (concat "gBrowser.reloadTab( emacs_tab );" )) ;; 更新可

 		 (tt3-mozrepl-request (concat "emacs_tab.url;" )) ;; 複製可

		 )

	 (progn ;; TEAMGEAR
 		 (tt3-mozrepl-request "gBrowser.selectedTab = gBrowser.loadOneTab('http://www.teamgear.net/teamgear/TG/top/simple.php'); ")
		 (sleep-for 0.5)
 		 (tt3-mozrepl-request "gBrowser.contentDocument.frm01.VID.value = 'EGASHIRA_CBRI';")
		 (sleep-for 0.5)
 		 (tt3-mozrepl-request "gBrowser.contentDocument.frm01.VNAME.value = 'EGASHIRA_CBRI';")
		 (sleep-for 0.5)
 		 (tt3-mozrepl-request "gBrowser.contentDocument.frm01.PWD.value = 'egashira';")
		 (sleep-for 0.5)
 		 (tt3-mozrepl-request "gBrowser.contentDocument.frm01.submit();") )

	 (progn
		 ;; toppageからIDを得る
 		 (tt3-mozrepl-request (concat "var inhtm = gBrowser.contentDocument.getElementsByTagName('html')[0].innerHTML;"
																	"var regex = /SID=([^&]*)&/;"
																	"var match = regex.exec( inhtm );"
																	"var sid = match[1];"
																	"var regex = /UC=([^&]*)&/;"
																	"var match = regex.exec( inhtm );"
																	"var uc = match[1];"
																	"var regex = /TC=([^&]*)&/;"
																	"var match = regex.exec( inhtm );"
																	"var tc = match[1];" ))
		 (sleep-for 0.5)
		 ;; 週表示へ移動
		 (tt3-mozrepl-request "content.location.href = 'http://www.teamgear.net/teamgear/TG/tg/main/tg_schedule/frmWeekly.php?SID=' + sid + '&UC=' + uc + '&TC=' + tc + '&CSID='")
		 (tt3-mozrepl-request "content.location.href = 'http://www.teamgear.net/teamgear/TG/tg/main/tg_schedule/frmWeekly.php?SID=' + sid + '&UC=' + uc + '&TC=' + tc + '&STDATE=2013-04-28'")
		 ;; [5]以降にひとりづつデータが取れる 
 		 (insert (tt3-mozrepl-request "gBrowser.contentDocument.getElementsByTagName('tr')[7].innerHTML;")))
		 (sleep-for 0.5)
 		 (tt3-mozrepl-request "gBrowser.contentDocument.frm01.submit();")
		 
		 )
	 
	 (progn
 		 (tt3-mozrepl-request "gBrowser.selectedTab = gBrowser.loadOneTab('http://google.com'); ") ;; addTabも動く
 		 (tt3-mozrepl-request "gBrowser.selectedTab = gBrowser.loadOneTab('file:///E:/Dropbox/MyData/tt/2013/09/2013-09-19-145150/2013-09-19-145150.howm.html#sec-1-9'); ") ;; addTabも動く sec移動もOK
		 (sit-for 1)
		 (tt3-mozrepl-request "content.location.href = 'http://google.com'"))


 	 (tt3-mozrepl-request "gBrowser.selectedTab = gBrowser.tabs('2013-09-19-145150.howm'); ") ;; 動かない

	 (tt3-mozrepl-request "alert( gBrowser.contentDocument.getElementsByClassName('header-section-number').length );" ) ;; これで動くときと動かないときがある。
	 (tt3-mozrepl-request "var elements = gBrowser.contentDocument.getElementsByClassName('header-section-number'); alert( elements.length ); " )
	 (tt3-mozrepl-request "var elements = gBrowser.contentDocument.getElementsByClassName('header-section-number');  for( var i = 0; i<elements.length; ++i ){ alert( elements[i].innerHTML ); }" )

 	 (tt3-mozrepl-request "gBrowser.selectedTab = _getTabForContentWindow( document.getElementById('2013-09-19-145150.howm') );" )

	 (insert (tt3-mozrepl-request "window.getSelection(); "))
	 (tt3-mozrepl-request "goDoCommand('cmd_copy'); " )
	 (tt3-mozrepl-request "goDoCommand('cmd_selectWordNext'); " )
	 
	 (tt3-mozrepl-request "repl._workContext.content.location.href; ")
	 (tt3-mozrepl-request "repl._workContext.content.document.title; ")
)

(provide 'tt3-mozrepl)





