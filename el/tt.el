;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt.el
(setq debug-on-error t)
(require 'cl)
(require 'em-glob)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 開発上のルール
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 命名法：
;;   Publicコマンド : tt:xxxx-                 : 基本引数無
;;   Public関数     : thinktank3-xxxxx-
;;   Public変数     : thinktank3-xxxxx-
;;   Local関数      : tt3-xxxx-
;;   Local変数      : tt3-xxxx-
;;
;; ライブラリ構成：
;;   Public関数一覧
;;   Public変数一覧
;;
;; ルール
;;   コマンドからコマンドを呼ばない
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load library 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'tt-util)             ;; elisp, emacs
(require 'tt3-system)          ;; thinktank-resource
(require 'tt3-resource)        ;; thinktank-resource

(tt:resource-start-server)

;(thinktank3-config   :set-memodir)
;(thinktank3-property :initialize)
;(thinktank3-config   :initialize)

;(tt:start-webrick)             ;; QSyncのためか、ディレクトリ内に同名のファイルが存在することが発生する。

(require 'tt3-org)             ;; emacs, thinktank-system
(require 'tt3-misc)            ;; elisp, emacs
(require 'tt3-menu)            ;; thinktank-system
(require 'tt3-mode)            ;; thinktank-system
(require 'tt3-config)


(require 'tt3-mozrepl)         ;; addon

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 無し
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(thinktank3-menu-clear)
(thinktank3-menu-add '(( "M|Memo M|Home"   :command tt:resource-open-memo :help "トップ" )
											 ( "M|Memo S|Save"   :command save-buffer :help "保存" )
											 ( "M|Memo X|Close"  :command kill-buffer :help "閉じる")
											 ( "M|Memo D|Delete"     :command tt:resource-delete-memo   :help "削除" :context "ext:howm" )
											 ( "M|Memo N|New N|New"  :command tt:resource-open-new-memo :help "新規")
											 ( "M|Memo N|New L|Link" :command tt:resource-link-new-memo :help "新規メモへリンク" :context "ext:howm" )

											 ( "L|List A|All"    :command tt:resource-index :help "全メモ" )
											 ( "L|List S|Search" :command tt:resource-index-search :help "検索" )
											 ( "L|List R|Recent" :command tt:resource-index-recent :help "最近" )
											 
											 ( "B|Browser H|Home"   :func (tt:resource-browse-memo :memoid "0000-00-00-000000") :help "トップメモをブラウズ" )
											 ( "B|Browser M|Memo"   :func (tt:resource-browse-memo) :help "カレントメモをブラウズ" :context "ext:howm" )
											 ( "B|Browser R|Recent" :func (tt:resource-browse-memo :type "recent") :help "最近のメモ" )
											 ))

(thinktank3-menu-add '(( "M|Memo C|Clipboard" 
												 :helm ((name . "Memo") 
																(candidates . (("id" . :id) ("howm" . :howm) ("title" . :title) ("url" . :url) ("content" . :content) ("nodes" . :nodes)))
																(action . tt:misc-clipboard-memoinfo))
												 :help "メモ関連情報をコピーする" :context "ext:howm" )
											 ; ( "M|Memo W|winword" :context "ext:howm" :command tt:mozrepl-memo-docx :help "wordファイルを表示")

											 ( "I|Insert T|Today" :helm ((name . "Today") (candidates . (("Node" . :node) ("Text" . :text) ("Tag" . :tag))) (action . tt:misc-insert-today)) :help "日付を挿入")
											 ( "I|Insert K|Kyou"  :helm ((name . "Kyou")  (candidates . (("Node" . :node) ("Text" . :text) ("Tag" . :tag))) (action . tt:misc-insert-kyou))  :help "日付(J)を挿入")
											 ( "I|Insert N|Now"   :helm ((name . "Now")   (candidates . (("Node" . :node) ("Text" . :text) ("Tag" . :tag))) (action . tt:misc-insert-now))   :help "現在時を挿入")
											 ( "I|Insert X|TT"    :helm ((name . "TT")    (candidates . (("Node" . :node) ("Text" . :text) ("Tag" . :tag))) (action . tt:misc-insert-tt))    :help "Thinktankタグを挿入")
											 ( "I|Insert F|Firefox" :command tt:mozrepl-insert-current-page-link :help "firefoxのcurrent pageへのlinkを挿入" )
											 ))

(thinktank3-menu-add '(;( "@|misc C|Calendar C|Show" :command tt:calfw-show :help "カレンダーを開く")
											 ;( "@|misc C|Calendar G|SelectGroup" :command tt:calfw-show-group :help "カレンダーグループを開く")
											 ;( "@|misc C|Calendar O|ShowOne" :command tt:calfw-show-one :help "カレンダーを一種類のみ開く")
											 ;( "@|misc C|Calendar U|Update" :command tt:calfw-update :help "カレンダーグループを選択して更新する")
											 ;( "@|misc C|Calendar S|SetCurrent" :command tt:calfw-select :help "表示カレンダーを選択する")

											 ( "U|Usability F|Footnote O|Open-file" :command tt:org-open-footnote-file :help "footnote末尾のファイルを開く" )
											 ( "U|Usability F|Footnote P|Open-pubmed" :command tt:org-open-footnote-site :help "fornoteの引用論文をpubmedで検索" )

											 ( "U|Usability E|Emacs H|Helm-imenu" :command helm-imenu :help "helmでimenu")
											 ( "U|Usability E|Emacs R|Helm-regexp" :command helm-regexp :help "helmでregexp")
											 ( "U|Usability E|Emacs I|Isearch-occur" :command isearch-occur :help "検索")
											 ( "U|Usability E|Emacs R|Regexp-Builder" :command regexp-builder :help "Regexp Builder")
											 
											 ( "U|Usability U|View A|Set-Alpha" :command tt:set-alpha :help "透過度を設定する")
											 ( "U|Usability U|View T|Toggle-Toolbar" :command tt:misc-toggle-toolbar :help "toolbar表示をトグル" )
											 ( "U|Usability U|View M|Toggle-Menubar" :command tt:toggle-menubar :help "menubar表示をトグル" )
											 ( "U|Usability U|View S|Toggle-Scrollbar" :command tt:misc-toggle-scrollbar :help "scrollbarをトグル" )
											 ( "U|Usability U|View F|Toggle-Fullscreen" :command tt:toggle-full :help "fullscreenをトグル" )
											 ( "U|Usability U|View X|Toggle-Maximize" :command tt:toggle-expand :help "最大化をトグル" )
											 ( "U|Usability U|View B|Text-Bending" :command tt:truncate-lines :help "折り畳みをトグル" )
											 ( "U|Usability U|View I|Indent" :command tt:indent-all :help "インデントをそろえる" )
											 ( "U|Usability U|View H|Word-Hilight-on" :command tt:highlight-word :help "ハイライトする" )
											 ( "U|Usability U|View O|Word-Hilight-off" :command tt:unhilight :help "ハイライトを止める")	))

(thinktank3-menu-add '(( "S|System W|Restart-webrick" :command tt:system-run-webrick :help "local webrick serverを起動する" )
											 ( "S|System D|Directory" :command tt:util-open-directory :help "ディレクトリを開く" )
											 ( "S|System L|Reload-memo" :command tt:resource-reload :help "メモをリロードする" )
											 ( "S|System S|Restruct-memo" :command tt:resource-restruct :help "メモをリロードする" )
											 ( "S|System P|Replace-memo" :command tt:resource-replace :help "メモをリロードする" )
											 ( "S|System O|Object" :command tt:resource-show-property-object :help "objectの数を数える" )
											 ( "S|System A|AutoSelect" :command tt:menu-select-one-automatically :help "helmで１つに絞り込んだら自動選択")
											 ( "S|System M|Menu-Initialize" :command tt:menu-initialize :help "menuを更新する" )

											 ( "O|OtherMenu L|UrlList" :command tt:resource-show-web-url-list :help "URLリスト" )
											 ( "O|OtherMenu T|UrlTree" :comman tt:resource-show-web-url-tree :help "URLリスト" )

											 ( "h|Help D|Document H|Help" :command help :help "ヘルプ")
											 ( "h|Help D|Document U|Url" :info url :help "urlのマニュアル")
											 ( "h|Help D|Document S|Eshell" :info eshell :help "eshellのマニュアル")
											 ( "h|Help D|Document O|Orgmode" :info org :help "orgのマニュアル")
											 ( "h|Help D|Document C|Cl" :info cl :help "clのマニュアル")
											 ( "h|Help D|Document E|Emacs" :info emacs :help "emacsのマニュアル")
											 ( "h|Help D|Document L|Elisp" :info elisp :help "elispのマニュアル")
											 ( "h|Help T|Test F|File" :func (tt3-menu-show-keymap-menu menu-bar-files-menu :list) :help "ファイルメニュー" )
											 ( "h|Help T|Test O|Options" :func (tt3-menu-show-keymap-menu menu-bar-options-menu :list) :help "オプションメニュー" )
											 ( "h|Help T|Test M|Manuals" :func (tt3-menu-show-keymap-menu menu-bar-manuals-menu :list) :help "マニュアルメニュー" )
											 ( "h|Help C|Context" :func (msgbox "context:%S" tt3-menu-context) :help "contextを表示")
											 ( "h|Help R|Ruby M|Rurima" :command tt:mozrepl-rurema-search :help "るりまさーち")
											 ( "h|Help R|Ruby R|RubyRef" :command tt:mozrepl-rubyref-search :help "Rubyリファレンス" )

											 ))


(defun tt-hook-search-engine-to-menu ()	;; web search engine の登録
	(mapcar-tt3-property-subnode "Menu.WebSearch"
															 (let ((menu (org-entry-get nil "menu"))
																		 (url  (org-entry-get nil "url")) 
																		 (help (org-entry-get nil "help")))
																 (when menu (when (string-match "\\[\\[\\([^\]]+\\)\\]\\[\\([^\]]+\\)?\\]\\]" url)
																							(setq help (concat (when help (concat help ": ")) (match-string 2 url)))
																							(setq url  (match-string 1 url)))
																			 (thinktank3-menu-add `(,menu
																															:url     ,url
																															:help    ,help
																															:input   ,(intern (org-entry-get nil "input"))
																															:context ,(org-entry-get nil "context")
																															:message ,(org-entry-get nil "message")))))))
(add-hook 'thinktank3-menu-after-initialize-hook 'tt-hook-search-engine-to-menu)


(defun tt-hook-query-to-menu ()	;; Query 関連の登録
	(mapcar-tt3-property-subnode "Menu.Query"
															 (let* ((menu (org-entry-get nil "menu"))
																			(help (org-entry-get nil "help")))
																 (when menu (thinktank3-menu-add `(,menu
																																	 :func    (tt:resource-index :name ,title)
																																	 ;; :func    (tt:resource-index :name ,menu)
																																	 :help    ,help
																																	 :context ,(org-entry-get nil "context")))))))
(add-hook 'thinktank3-menu-after-initialize-hook 'tt-hook-query-to-menu)

(defun tt-hook-mozrepl-to-menu ()	;; MozRepl 関連の登録
	'(mapcar-tt3-property-subnode "Menu.Mozrepl"
																(let* ((menu (org-entry-get nil "menu"))
																			 (help (org-entry-get nil "help"))
																			 (code (trim-string (tt3-tt3-property-get-element :src-block))))
																	(when menu (thinktank3-menu-add `(,menu
																																		:func    (tt3-mozrepl-request ,code)
																																		:help    ,help))))))
(add-hook 'thinktank3-menu-after-initialize-hook 'tt-hook-mozrepl-to-menu)

(defun tt-hook-oneline-to-menu ()	;; 一行メモの関数定義＆登録
	;; エラーでる。
	'(loop for ( memoid name comm supl menu ) in (tt:resource-index :name "Menu.Query.AssociateFile.oneline" :output :lisp)
				 unless (equal menu "")
				 do (thinktank3-menu-add `(,menu :func (tt3-resource-append-oneline :memoid ,memoid :name ,name) :help ,(concat "一行メモ:" name)))))
(add-hook 'thinktank3-menu-after-initialize-hook 'tt-hook-oneline-to-menu)


(tt:menu-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WebSearchEnginesをorg-link-abbrev-alistに登録
;; 文中 [[google:orexin leptin]] で検索できるようになる。
(setq org-link-abbrev-alist nil)
(mapcar-tt3-property-subnode "Menu.WebSearch" 
														 (when (org-entry-get nil "orgtag")
															 (push (cons (org-entry-get nil "orgtag") (replace-regexp-in-string "%input" "%s" (org-entry-get nil "url"))) org-link-abbrev-alist)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to monitor thinktank.log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'find-file-hook (lambda () (when (string-match "log$" (buffer-name)) (auto-revert-tail-mode t))))
(add-hook 'after-revert-hook (lambda () (when auto-revert-tail-mode (end-of-buffer))))


(provide 'tt)




