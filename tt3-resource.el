;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-user.el  thinktank
(require 'url)
(require 'helm-config)
(require 'helm)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (tt:start-webrick)
;; 
;; (tt:resource-index &rest plist )
;;
;; (tt:resource-reload)           (interactive)
;; (tt:resource-restruct)         (interactive)
;; (tt:resource-show-top-memo)    (interactive)
;; (tt:resource-destroy-memo)     (interactive)
;; (tt:resource-create-memo-from-region) (interactive) 
;; (tt:resource-create-memo)      (interactive)
;; (tt:resource-create-memo-link) (interactive)
;; (tt:resource-update-memo)      (interactive)
;; (tt:resource-major-version-up) (interactive)
;; (tt:resource-minor-version-up) (interactive)
;; 
;; thinktank-memo
;; thinktank-tag
;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; コマンド 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; webrick serverを走らせる
;;
(defun tt:start-webrick () (interactive) (tt3-resource-start-webrick "version2.1"))

(defun tt3-resource-start-webrick ( &optional option ) (interactive) "* [説明] webrickを起動する。"
	(unless (tt:resource-check-server) ;; local server未起動時に実行
		
		;; w32/nsではtt.elのディレクトリにバッチファイルを作成する。
		(setq default-directory (file-name-directory (locate-library "tt.el"))) 
		
		;; emacs子プロセスではなく、起動したshell窓内でserver起動する

		;; ruby thinktank.rb --memodir "D:/Dropbox/MyData/tt/" --test test


		(case (window-system)
			('w32 (defun set-shell-cmdproxy()
							(interactive)
							(setq shell-file-name "cmdproxy")
							(setq explicit-shell-file-name "cmdproxy")
							(setenv "SHELL" explicit-shell-file-name)
							(setq w32-quote-process-args t)
							(setq shell-command-switch "-c")
							)
						(set-shell-cmdproxy)
						(with-temp-file (concat default-directory "thinktank.bat")
							(insert (format "%s:\n" (substring default-directory 0 1))
											(format "cd %s\n" default-directory)
											(format "ruby thinktank.rb %s" option)))
						(start-process-shell-command "thinktank-server" nil "start cmd.exe /k thinktank.bat"))
			;;(shell-command "start cmd.exe /k thinktank.bat"))
			
			('ns  (with-temp-file (concat default-directory "thinktank.applescript")
							(insert "tell application \"Terminal\"\n"
											"do script \"echo dummy\"\n"
											(format "do script \"cd %s\" in window 1\n" default-directory)
											(format "do script \"ruby thinktank.rb %s\" in window 1\n" option)
											"end tell\n"))
						(cd default-directory)
						(shell-command (format "/usr/bin/osascript %sthinktank.applescript" default-directory)))
			
			('x (start-process-shell-command "xfce" nil (format "xfce4-terminal --working-directory='%s' -H --command='ruby thinktank.rb %s'" default-directory option))))))

;;
;; メモを列挙する
;;
(defun tt:resource-index ( &rest plist ) 
"* [説明] thinktank serverへlookupを投げ、結果を得る。
  [引数] lookup   :  検索式                                                         (vector JSON文字列)
         name     :  system memoのExtension.Queries.(name)のnodeから他paramを得る。 (文字列) (:memo-synchronize, :memo-initialize)
         input    :  ユーザー入力を得て、lookup式中の%sを置換する。                 (:select :point :clip :key :cursord :emacs :any 文字列)
         message  :  emacsでキー入力する場合のメッセージ                            (文字列)
         senddata :  サイズの大きいデータを送る場合はsenddataを介して送信する。     (:current-memo :buffered-memos :displayed-memos :nodata(def) 文字列 ファイル名)
         cache    :  基本的に結果ファイルはキャッシュされ、次回以降それを使う。     (:no :daily :once(def))
         output   :  結果をどのような形式で返すか                                   (:text :list :lisp :source :helm(def))
         attrib   :  helm表示のためのフラグ                                         (:tag :memo(def))
  [主な使用例]
　　　　 1) system memo #+BEGIN_SRC thinktank 〜 #+END_SRC を実行する。
         2) 関数で実行   ex> (tt:resource-index2 :name ''Misc.MemoObject'')
            menuには関数実行形式で登録してある
  [注意] current-memoを送信した場合、結果はmemoのdirectoryに格納される。 "
	(interactive)
	(catch 'arrest
		(let* (lookup name senddata cache message input output attrib lower upper
									(cachefile (thinktank3-config :tempdir))) ;; デフォルトはtmpdir
			
			;; 各種パラメータ取得
			(setq name     (getf plist :name))
			(setq lookup   (or (getf plist :lookup)   (thinktank3-property name :src-block)))
			(setq senddata (or (getf plist :senddata) (thinktank3-property name "senddata") :nodata))
			(setq cache    (or (getf plist :cache)    (thinktank3-property name "cache") :once))
			(setq message  (or (getf plist :message)  (thinktank3-property name "message") "input:"))
			(setq input    (or (getf plist :input)    (thinktank3-property name "input")))
			(setq output   (or (getf plist :output)   (thinktank3-property name "output") :helm))
			(setq attrib   (or (getf plist :attrib)   (thinktank3-property name "attrib") :memo))
			(setq lower    (or (getf plist :lower)    (thinktank3-property name "lower") ""))
			(setq upper    (or (getf plist :upper)    (thinktank3-property name "upper") ""))

			;; 中断処理
			(unless name (throw 'arrest "no name"))
			(case name ((:memo-synchronize :memo-initialize) 
									(tt3-resource-index-memo-http-request :action name)
									(throw 'arrest (symbol-to-string name))))

			;; スイッチ類のシンボル化
			(when (stringp cache)  (setq cache  (intern cache)))
			(when (stringp output) (setq output (intern output)))
			(when (stringp attrib) (setq attrib (intern attrib)))

			;; ユーザー入力取得 ( input と senddata ) 
			(setq input (cond ((stringp input)
												 (cond ((equal ":" (substring input 0 1))
																(tt3-tt3-menu-get-input (intern input) message))
															 (t input)))
												(t nil)))
			(setq lookup (if (stringp lookup) lookup (json-encode lookup)))
			(setq lookup (replace-regexp-in-string "%input" input lookup))
			(setq lookup (replace-regexp-in-string "%upper" upper lookup))
			(setq lookup (replace-regexp-in-string "%lower" lower lookup))

			(when (stringp senddata)
				(setq senddata (cond ((file-exists-p senddata) 
															(with-temp-buffer (insert-file-contents senddata) (buffer-substring-no-properties (point-min) (point-max))))
														 ((string-match "[0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]" senddata)
															senddata)
														 (t
															(intern senddata)))))
			(when (symbolp senddata)
				(setq senddata (case senddata
												 (:nodata         nil)
												 (:current-memo   (let ((bufname (or (thinktank3-menu-buffer-on-useraction) (buffer-name))))
																						(setq cachefile (thinktank3-format :memodir bufname )) ;; cacheの格納先変更
																						bufname))
												 (:buffered-memos (join-string (loop for buf in (buffer-list)
																														 for name = (buffer-name buf)
																														 if (string-match "\.howm$" name)
																														 collect name)))
												 (:displayed-memos (buffer-string)))))

			;; server response保存ファイルの再作成

			(setq cachefile (concat cachefile name (if input (format "--%s--" input) "") ".lhowm" ))

			;; (msgbox "INDEX:%s   %s" (tt3-tt3-menu-context-prefix) cachefile)
			;; (popup-tip (format "cachefile:%s\nname:%s\nlookup:%s\ninput:%s\nattrib:%s\nmessage:%s\ncache:%s\nsenddata:%80s" cachefile name lookup input attrib message cache senddata)) ;; 矢印キーで表示閉じる

			(when (or (null (file-exists-p cachefile))        ;; Cacheファイル無い場合
								(< 0 (tt3-tt3-menu-context-prefix))     ;; C-u指定ある場合
								(equal cache :no)                       ;; Cache指定noの場合
								(and (equal cache :daily)               ;; Cache指定dailyで既存が本日作成されていない場合
										 (string< (format-time-string "%Y-%m-%d" (nth 5 (file-attributes cachefile))) (format-time-string "%Y-%m-%d")))) ;; (nth 5 (file-attributes "tt.el")) (current-time) 
				
				; (delete-file cachefile)
				(with-temp-file cachefile (insert-buffer-substring (assoc-default "Buffer" (tt3-resource-index-memo-http-request :lookup lookup :body senddata))))) ;; server通信＆データcache

			;; helm sourceの作成
			;; 値を返す or helm実行 
			(case output
				((:text :list :lisp) (let ((text (with-temp-buffer (insert-file-contents cachefile) (buffer-substring-no-properties (point-min) (point-max)))))
															 (case output
																 ;;(:lisp (car (read-from-string (concat "(" text ")"))))
																 (:lisp (loop for item in (split-string text "\n")
																							for valid-item = (condition-case nil (car (read-from-string item)) (error nil)) ;; captionにbackslashを含むとread-from-stringでerrorになって表示されなくなる。
																							if valid-item 
																							collect valid-item ))
																 (:list (split-string text "\n"))
																 (:text text))))
				(:no nil)
				(t (let* ((source `((name . ,(upcase name))
														(candidates-in-buffer)
														(init . (lambda () (with-current-buffer (helm-candidate-buffer 'global) (insert-file-contents ,cachefile))))
														(header-name . (lambda (x) ,name))
														(type . ,(case attrib
																			 (:memo 'thinktank-memo)
																			 (:tag 'thinktank-tag))))))
						 (case output
							 (:source source)
							 (t       (helm :sources source :buffer (if name (format "*%s*" name) "*index*"))))))))))




;;
;; サーバーをチェック
;;
(defun tt:resource-check-server () "メモサーバーをチェック"
	(interactive)
	(if (string= "HTTP/1.1 200 OK " (assoc-default "Status" (tt3-resource-http-request :resource :memos :action :show :id "0000-00-00-000000")))
			(progn (message "webrick server is already starting") t)
		(progn (message "no thinktank webrick server exists") nil)))
;; (documentation 'tt:resource-check-server)

;;
;; メモを再構築する
;;
(defun tt:resource-reload () "メモを再構築" (interactive) (tt:resource-index :name :memo-initialize))

;;
;; メモを更新する
;;
(defun tt:resource-restruct () "メモを更新" (interactive)	(tt:resource-index :name :memo-synchronize))
																				; system-propertyを設定する
																				;(defun thinktank3-resource-update-system-property ( key value )
																				;	(with-current-buffer 
																				;		(assoc-default "Buffer" (tt3-local-maintain-memo :syskey key :value value))
																				;		(buffer-string)))
																				;
																				;'((thinktank3-resource-get-system-property "OAuth2.GoogleCalendar.not-requred"))
																				;'((thinktank3-resource-update-system-property "OAuth2.GoogleCalendar.not-requred" "100"))

;;
;; トップメモを開く
;;
(defun tt:resource-show-top-memo () "トップメモを開く" (interactive) (tt3-resource-show-memo :memoid "0000-00-00-000000"))

;;
;; メモを削除
;;
(defun tt:resource-destroy-memo () "メモを削除" (interactive)	(tt3-resource-destroy-memo :memoid (thinktank3-format :memoid (buffer-name))) (kill-buffer))

;;
;; メモを保存
;;
(defadvice save-buffer (around tt:resource-save-memo activate) "メモを保存"
	;; createの場合 (org-capture) 内部で使用されるsave-bufferからcallされる。
	(cond ((and (string-match "howm$" (buffer-name)) (null (buffer-file-name)))
				 (message "saving howm file")
				 (let ((updatep (if (file-exists-p (thinktank3-format :memopath (buffer-name))) :replace :create)))
					 (tt3-resource-save-memo :memoid (thinktank3-format :memoid (buffer-name))
																	 :content (buffer-string)
																	 :mode    updatep)))
				(t ad-do-it)))

'((ad-disable-advice 'save-buffer 'around 'tt:resource-save-memo))

;;
;; 選択範囲からメモ作成
;;
(defun tt:resource-create-memo-from-region () "選択範囲からメモ作成"
	(interactive) 
	(when (use-region-p) (let ((res (tt3-resource-save-memo :content (buffer-substring (region-beginning) (region-end)) :mode :create)))
												 (switch-to-buffer (assoc-default "Buffer" res))
												 (rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
												 (thinktank-minor-mode 1)
												 (goto-char (point-min)))))

;;
;; メモを作成
;;
(defun tt:resource-create-memo () "メモを作成" (interactive)
	(let* ((template-memo    (replace-regexp-in-string "\\\\n" "\n" (thinktank3-config :template-memo)))
				 (template-oneline (thinktank3-config :template-oneline))
				 (res (tt3-resource-save-memo :content (format-time-string template-memo) :mode :create)))

		(switch-to-buffer (assoc-default "Buffer" res))
		(rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
		(thinktank-minor-mode 1)
		(goto-char (point-min))
		(search-forward "?") (backward-char) (delete-char 1)
		))

;;
;; 新規メモを作成・保存 (new.howm)
;;
(defun tt:resource-create-memo-link () (interactive)
	(let ((created (thinktank3-format :memofile :now)))
		(narrow-to-region (progn (end-of-line) (point)) (progn (beginning-of-line) (point)))
		(when (re-search-forward "new\.howm") (replace-match created))
		(widen)
		(save-buffer)
		(tt3-resource-save-memo :memoid created
														:content (format "no content\n%s" created)
														:mode :create
														:show t)))

;;
;; 現メモを保存
;;
(defun tt:resource-update-memo () (interactive)
	(tt3-resource-save-memo :memoid  (thinktank3-format :memoid (buffer-name)) 
													:content (buffer-string)
													:mode    :replace))

;;
;; 現メモをマイナー version up
;;
(defun tt:resource-major-version-up () (interactive) 
	(tt3-resource-save-memo :memoid  (thinktank3-format :memoid (buffer-name))
													:content (buffer-string)
													:verup   :major
													:mode    :feedback))

;;
;; 現メモをメジャー version up
;;
(defun tt:resource-minor-version-up () (interactive)
	(tt3-resource-save-memo :memoid  (thinktank3-format :memoid (buffer-name))
													:content (buffer-string) 
													:verup   :minor
													:mode    :feedback))

;;
;; メモを削除する
;;
(defun tt3-resource-destroy-memo ( &rest params )
	(let* ((memoid (plist-get params :memoid)))
		(tt3-resource-http-request :resource :memos
															 :action   :destroy
															 :id       memoid )))
	
	;; (let* ((res (eval (push 'tt3-resource-destroy-memo-http-request params))))))


																				; メモを読む：　バッファーをアクティベートしない
(defun tt3-resource-open-memo ( &rest params ) (interactive)
	(let* ((memoid   (plist-get params :memoid))
				 (name     (plist-get params :name))
				 (memofile (thinktank3-format :memofile memoid)))
		(or (and memofile (get-buffer memofile))
				(let (res)
					(setq res (tt3-resource-http-request :resource :memos
																							 :action   :index
																							 :query    `((:lookup . [(:AND "object" :type "thinktankproperty")
																																			 (:AND "search" :target "name" :text ,name)
																																			 (:RESPONSE "first" :type "memo")]))))
					;; (setq res (tt3-resource-open-memo-http-request :memoid memoid :name name))
					(or (get-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
							(with-current-buffer (assoc-default "Buffer" res)
								(rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
								(thinktank-minor-mode 1)
								(current-buffer)))))))


																				;
																				; メモを表示する：　バッファーをアクティベートする
																				;
																				; * 自作popupメニューから開く (tt3-menu.el)    ex) 0000-00-00-000000.howm 専用 
																				; * helmのメニューから開く (下; open memos)
																				; * button-lockから開く (tt3-mode.el で定義)   ex) xxxx-xx-xx-xxxxxx.howm
																				; * org-linkから開く (tt3-org.elで定義)        ex) [tt:xxxx-xx-xx-xxxxxx.howm::xxxxxx]
																				;
(defun tt3-resource-show-memo ( &rest params ) (interactive)
	(let* ((memoid   (plist-get params :memoid))
				 (hilight  (plist-get params :hilight))
				 (jump     (plist-get params :jump))
				 (memofile (thinktank3-format :memofile memoid))
				 res)

		(cond ((get-buffer memofile)
					 (switch-to-buffer memofile))
					(t
					 ;; (setq res (tt3-resource-open-memo-http-request :memoid memoid))
					 (setq res (tt3-resource-http-request :resource :memos
																								:action   :show
																								:id       (thinktank3-format :memoid memoid)))
					 (switch-to-buffer (assoc-default "Buffer" res))
					 (rename-buffer (thinktank3-format :memofile (assoc-default "Filename" res)))
					 (thinktank-minor-mode 1)))
		
		(cond (jump
					 ;;
					 ;; 位置指定ジャンプ
					 ;;
					 ;; [[tt:0000-00-00-000000.howm::255]]              :ポジション指定jump
					 ;; [[tt:0000-00-00-000000.howm::My Target]]        :検索文字列指定jump
					 ;; [[tt:0000-00-00-000000.howm::*My Target]]       :node指定jump
					 ;; [[tt:0000-00-00-000000.howm::/regexp/]]         :検索正規表現指定jump
					 ;;
					 (when (numberp jump) (setq jump (number-to-string jump)))
					 (when (string-match "^::" jump) (setq jump (substring jump 2))) ; '::' を削る

					 ;; (beginning-of-buffer)
					 (cond ((< 0 (string-to-number jump))              ; 数値
									(goto-char (+ 1 (string-to-number jump))))
								 
								 ((string-match "^/.*" jump)                 ; 正規表現
									(tt:highlight-word :regexp (substring jump 1 -1))
									(re-search-forward (substring jump 1 -1)))
								 
								 ((string-match "^\*.*" jump)                ; node
									(tt:highlight-word :regexp (concat "^\*+[ \t　]*" (substring jump 1)))
									(re-search-forward (concat "^\*+[ \t　]*" (substring jump 1 -1)))
									(beginning-of-line))
								 
								 (t                                          ; 文字列
									(tt:highlight-word :keyword jump)
									(or (search-forward jump nil t) (progn (goto-char (point-min)) (search-forward jump nil t)))
									(kill-new jump))))
					
					(hilight
					 (tt:highlight-word :keyword hilight)))

		(ignore-errors
			(redraw-display)
			(loop for view in (split-string (org-entry-get (point-min) "initial-view") "\[, \]")
						do (cond ((string= view "window:full")    (w32-send-sys-command #xf030))
										 ((string= view "window:nonfull") (w32-send-sys-command 61728))
										 ((string= view "menu:t")   (menu-bar-mode 1))
										 ((string= view "menu:nil") (menu-bar-mode 0))
										 ((string= view "tool:t")   (tool-bar-mode 1))
										 ((string= view "tool:nil") (tool-bar-mode 0))
										 ((string= view "scroll:t")   (scroll-bar-mode 1))
										 ((string= view "scroll:nil") (scroll-bar-mode 0))
										 ((string= view "folding:t")  (setq truncate-lines nil))
										 ((string= view "folding:nil") (setq truncate-lines t))))
			(redraw-display))



		(ignore-errors
			(macrolet ;; マクロである必要がある
					((tt3-resource-show-memo-fcolor-keyword () (let* ((fcolors (mapcar (lambda (x) (split-string x "[ :]"))
																																						 (split-string (org-entry-get (point-min) "fcolor-keyword") ","))))
																											 (if fcolors `(progn
																																			,@(mapcar (lambda (x) `(defface 
																																															 ,(intern (format "ttf-%s-%s" (car x) (buffer-name)))
																																															 '((t (:foreground ,(car x) :bold t :line t))) nil)) fcolors)
																																			,@(mapcar (lambda (x) `(font-lock-add-keywords nil 
																																																										 '((,(concat "\\(" (mapconcat 'regexp-quote (cdr x) "\\|") "\\)") . 
																																																												',(intern (format "ttf-%s-%s" (car x) (buffer-name))))))) fcolors)
																																			))))
					 (tt3-resource-show-memo-bcolor-keyword () (let* ((bcolors (mapcar (lambda (x) (split-string x "[ :]"))
																																						 (split-string (org-entry-get (point-min) "bcolor-keyword") ","))))
																											 (if bcolors `(progn
																																			,@(mapcar (lambda (x) `(defface 
																																															 ,(intern (format "ttb-%s-%s" (car x) (buffer-name)))
																																															 '((t (:background ,(car x)))) nil)) bcolors)
																																			,@(mapcar (lambda (x) `(font-lock-add-keywords nil 
																																																										 '((,(concat "\\(" (mapconcat 'regexp-quote (cdr x) "\\|") "\\)") . 
																																																												',(intern (format "ttb-%s-%s" (car x) (buffer-name))))))) bcolors)
																																			)))))
				
				(tt3-resource-show-memo-fcolor-keyword)
				(tt3-resource-show-memo-bcolor-keyword)))
		(font-lock-fontify-buffer)
		(current-buffer)))

																				; メモを保存する
																				;
(defun* tt3-resource-save-memo ( &key memoid content verup name show mode url-link coment) (interactive)
				(let* (res)

					(setq content (encode-coding-string content 'utf-8-unix))
					;; (setq res (tt3-resource-save-memo-http-request :memoid memoid :content content :verup verup :name name))
					(setq res (tt3-resource-http-request :resource :memos
																							 :action   :update
																							 :id       (or (thinktank3-format :memoid memoid)
																														 (thinktank3-format :memoid :now))
																							 :body     (cond ((stringp content) content)
																															 ((get-buffer content) (with-current-buffer content (buffer-string))))
																							 :query    `((:optional . (:verup ,verup :name ,name)))))
					
					(case mode
						(:create (let ((filename (thinktank3-format :memofile (assoc-default "Filename" res))))
											 (when show 
												 (switch-to-buffer (assoc-default "Buffer" res))
												 (rename-buffer filename)
												 (thinktank-minor-mode 1)
												 (current-buffer))))

						(:feedback (let ((pos (point)))
												 (switch-to-buffer (thinktank3-format :memofile memoid))
												 (erase-buffer)
												 (insert-buffer-substring-no-properties (assoc-default "Buffer" res))
												 (goto-char pos)))

						(:append (let ((filename (thinktank3-format :memofile (assoc-default "Filename" res))) pos)
											 (when (get-buffer filename)
												 (set-buffer (thinktank3-format :memofile filename))
												 (setq pos (point))
												 (erase-buffer)
												 (insert-buffer-substring-no-properties (assoc-default "Buffer" res))
												 (goto-char pos))))
						
						(:replace (message "REPLACED")))
					
					res ))




;;
;;
;;
(defun tt3-resource-append-oneline ( &rest params )
	(let* (comm supl
							(memoid (thinktank3-format :memoid (plist-get params :memoid)))
							(name   (thinktank3-format :name   (plist-get params :name)))
							template plist)
		(with-current-buffer (tt3-resource-open-memo :memoid memoid :name name) ;; :name name
			(setq template (format-time-string (org-entry-get (point-min) "thinktank-template")))
			(setq plist (loop for item in (split-string-and-unquote (org-entry-get (point-min) "thinktank-memo") ",")
												collect (split-string (trim-string item) ":")))

			(setq name (car (assoc-default "name" plist))) ;; 名前
			(setq comm (car (assoc-default "comm" plist))) ;; 入力時のメッセージ
			(setq supl (car (assoc-default "supl" plist))) ;; 追加情報
			(setq template (replace-regexp-in-string "%comm%" (read-string (or comm (format "comment for %s:" name))) template))
			(setq template (replace-regexp-in-string "%supl%" (cond ((equal supl "url")      (thinktank3-mozrepl-get-current-page-link))
																															((equal supl "clip")     (clipboarded-string))
																															((equal supl "calendar") (format "<%s>" (org-read-date nil nil nil nil
																																																										 (thinktank3-menu-calendar-on-useraction)
																																																										 "")))
																															(t ""))
																							 template))
			(beginning-of-buffer)
			(re-search-forward "^$")
			(insert (format "\n%s" (replace-regexp-in-string "\\\\n" "\n" template)))
			(tt:resource-update-memo))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm型の定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
;; thinktank-memo型
;;--------------------------------------------------------------------------------------------------------------------------------------------
(define-helm-type-attribute 'thinktank-memo
	'((action ("O|Open Memos"    . (lambda (x) (loop for item in (helm-marked-candidates)
																									 for ( memoid . jump ) = (thinktank3-format :memolink item)
																									 do  (tt3-resource-show-memo :memoid memoid :jump jump))))
						("M|Squeeze More"  . (lambda (x) (tt:resource-index :name "Search" :senddata (mapconcat 'identity (helm-marked-candidates) "\n"))))
						("L|insert Lines"  . (lambda (x) (mapc (lambda (item) (insert (thinktank3-format :memofile item) "\n")) (helm-marked-candidates))))
						("I|Insert Ids"    . (lambda (x) (mapc (lambda (item) (insert (thinktank3-format :memofile item) "\n")) (helm-marked-candidates))))
						("D|Delete Memos"  . (lambda (x) (mapc (lambda (item) (tt3-resource-destroy-memo :memoid (thinktank3-format :memoid item))) (helm-marked-candidates))))
						("F|File Manager"  . (lambda (x) (mapc (lambda (item) (open-directory-with-os-filemanager (thinktank3-format :memodir item))) (helm-marked-candidates))))
						("S|Shell"         . (lambda (x) (mapc (lambda (item) (open-directory-with-os-shellwindow (thinktank3-format :memodir item))) (helm-marked-candidates))))
						("H|Html"          . (lambda (x) (loop for item in (helm-marked-candidates)
																									 for ( memoid . jump ) = (thinktank3-format :memolink item)
																									 do  (tt:mozrepl-browse-memo memoid))))
						("E|diered"        . (lambda (x) (dired (thinktank3-format :memodir (car (helm-marked-candidates))))))
						)
		(action-transformer . (lambda ( actions candidate )
														(cond ((string-match "type:oneline" candidate) (cons '("R|Record One Line" . (lambda (x) (tt3-resource-append-oneline :memoid x) t)) actions)) ;; 一行メモの場合のaction
																	(t actions))))
		(candidate-number-limit . 1000 )))

(defun tt3-resource-thinktank-memo-action-transformer ( actions candidate )
	(cond ((string-match "type:oneline" candidate) (cons '("R|Record One Line" . (lambda (x) (tt3-resource-append-oneline :memoid x) t)) actions)) ;; 一行メモの場合のaction
				(t actions)))

;;--------------------------------------------------------------------------------------------------------------------------------------------
;; thinktank-tag型
;;--------------------------------------------------------------------------------------------------------------------------------------------
(define-helm-type-attribute 'thinktank-tag
	'((filtered-candidate-transformer . (lambda ( cands src )
																				(let* ((tagfmts '(("TODOTAG" "%s")
																													("ORGTAG" ":%s:")
																													("TITLETAG" "[%s]")
																													("PRIORITY" "#%s")))
																							 (fmt      (car (assoc-default (assoc-default 'name src) tagfmts))))
																					(loop for cand in cands
																								collect (destructuring-bind (key val)	(split-string cand ",")
																													(cons (format (concat fmt "(%s)") key val) (format fmt key)))))))
		(action ("L|insert"     . (lambda (x) (mapc (lambda (item) (insert item)) (helm-marked-candidates))))
						("F|find memos" . (lambda (x) (let* ((keyword (car (helm-marked-candidates))))
																						(helm :sources tt:resource-index-search-result :buffer "*search-keyword*")))))
		(candidate-number-limit . 1000 )))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; リソースへのアクセスのためのインターフェイス関数
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; http-requestを呼び出す関数
;;

;;(defun* tt3-resource-open-memo-http-request ( &key memoid name )
;;	(cond (memoid (tt3-resource-http-request :resource :memos
;;																					 :action   :show
;;																					 :id       (thinktank3-format :memoid memoid)))
;;				(name   (tt3-resource-http-request :resource :memos
;;																					 :action   :index
;;																					 :query    `((:lookup . [(:AND "object" :type "thinktankproperty")
;;																																	 (:AND "search" :target "name" :text ,name)
;;																																	 (:RESPONSE "first" :type "memo")]))))))
																				; ruby側 thinktank.get_memo( req.id )
																				; lisp側 examples
																				; (tt3-resource-open-memo-http-request :memoid "0000-00-00-000002")  ;; アクセス可、結果良
																				; (tt3-resource-open-memo-http-request :name "gtd-inbox")            ;; アクセス不可、結果不可


;;(defun* tt3-resource-save-memo-http-request ( &key memoid content verup name ) 
;;	(tt3-resource-http-request :resource :memos
;;													 :action   :update
;;													 :id       (or (thinktank3-format :memoid memoid)
;;																				 (thinktank3-format :memoid :now))
;;													 :body     (cond ((stringp content) content)
;;																					 ((get-buffer content) (with-current-buffer content (buffer-string))))
;;													 :query    `((:optional . (:verup ,verup :name ,name)))))
;;
  																			; ruby側 thinktank.update_memo!( req.id, req.body ) : id=nilのとき内部でcreate_memo呼び出す
																				;        thinktank.create_memo!( req.id, req.body ) : 直接には呼び出されない
																				; lisp側 examples
																				; (tt3-resource-save-memo-http-request :content "practice")
																				; (tt3-resource-save-memo-http-request :memoid "0000-00-00-000004" :content "practice")


;;(defun* tt3-resource-destroy-memo-http-request ( &key memoid )
;;	(tt3-resource-http-request :resource :memos
;;													 :action   :destroy
;;													 :id       memoid ))
																				; ruby側 thinktank.delete_memo!( req.id )
																				; lisp側 examples


(defun* tt3-resource-index-memo-http-request ( &key action body lookup    prop min max begin end keyword ) 
	(setq keyword (url-hexify-string (encode-coding-string (or keyword "") 'utf-8-unix)))
	(setq lookup (or lookup (case action
														(:calfw-timed-text  `[(:AND "object" :type "linetimetag")
																									(:AND "string" :prop "id" :min ,begin :max ,end)
																									(:RESPONSE "list" :type "linetimetag" :sort "id" :order "asc" 
																														 :display ":date \"[id]\" :link \"[memo.id].howm\" :jump \"[point]\" :caption \"[contents]\"")])
														(t nil))))
	(setq body (or body (case action 
												(:memo-initialize  "initialize-memo")
												(:memo-synchronize "synchronize-memo")
												(t ""))))
								 
	(when (stringp lookup) (setq lookup (json-read-from-string lookup)))

	(tt3-resource-http-request
	 :resource :memos
	 :action   :index
	 :body     body
	 :query    `((:lookup . ,lookup) (:optional . ()))))


																				; ruby側 thinktank.index_object( req.lookup, req.body )
																				; lisp側 examples
																				; (tt3-resource-index-memo-http-request :action :memo-initialize)  ;; アクセス可、結果良
																				; (tt3-resource-index-memo-http-request :action :memo-synchronize) ;; アクセス可、結果良


'((tt3-resource-http-request :resource :memos :action :index :query `((:lookup . ((:query . [ "[ThinktankChapter]" ] )
																																								(:list . "(memo.id).howm | (memo.title,%-30s) | memo:(title,%-30s)")
																																								(:limit . "30") (:offset . "0")
																																								(:sort . "address") (:order . "dsc")
																																								))))
	(tt3-resource-index-memo-http-request :lookup '((:query . [ "[ThinktankProperty]" "key == thinktank-memo" ] )
																					(:list . "(memo.id).howm | (key,%-20s) | (value,%-20s)")
																					(:sort . "value") (:order . "dsc")))

	(tt3-resource-index-memo-http-request :lookup '((:query . [ "[ThinktankChapter]" ] )
																					(:list . "(memo.id).howm | (memo.title,%-30s) | memo:(title,%-30s)")
																					(:limit . "30") (:offset . "0") (:sort . "address") (:order . "dsc")))

	(tt3-resource-index-memo-http-request :lookup '((:query . [ "[ThinktankMemo]" "content =~ orexin" ] )
																					(:list . "(id).howm | (weekday) | (title,%-30s)")
																					(:limit . "30")	(:offset . "0")	(:sort . "id") (:order . "dsc")))


	(tt3-resource-index-memo-http-request :action :memo-initialize)
	(tt3-resource-index-memo-http-request :action :memo-synchronize)
	)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; webrickへ直接アクセス。　http-requestを出す
;;

(require 'url)
(require 'json)


(defun* tt3-resource-http-request ( &key resource action id query body ) "
* [説明] webrick serverにアクセスする。
  [引数] resource :  :memos
         action   :  :index, :create, :new, :edit, :show, :update, :destroy, (:maintain, :analyze)
         id       :  \"xxxx-xx-xx-xxxxxx\"
         query    :  alist
         body     :  メモテキストやIDリスト等の参照用テキストデータ
  [返値] alistの形で response header が返る(下記)
         buffer(tt3-http-response-buffer)に response body が返る。
  [注意] win/macではscriptのあるディレクトリにバッチファイルを作成する。

	;; [返値]
  ;;
	;; ((\"Buffer\" . #<buffer *http-res*>)                                                    ; response body
	;;  (\"Status\" . \"HTTP/1.1 200 OK \")                                                    ; 
	;;  (\"Filename\" . \"E:/Dropbox/MyData/tt/0000-00-00-000000/0000-00-00-000000.howm\")     ; server上のファイル名 
	;;  (\"Content-Type\" . \"text/howm;charset=utf-8\")                                       ;
	;;  (\"Server\" . \"WEBrick/1.3.1 (Ruby/2.0.0/2013-05-14)\")                               ;
	;;  (\"Date\" . \"Wed, 23 Apr 2014 07:04:55 GMT\")                                         ;
	;;  (\"Content-Length\" . \"5749\")                                                        ;
	;;  (\"Connection\" . \"Keep-Alive\"))                                                     ;
	;; "
				
				(let* ((tt3-http-response-buffer "*http-res*")
							 url-request-data url-request-extra-headers tmpurl params )
					;;
					;; resource, action, id, query から url-request-method, url を得る
					;;
					(setq tmpurl (concat (thinktank3-config :baseurl)
															 (symbol-to-string resource)
															 (if id (concat "/" id) "")))
					
					;; for test
					(setq tmpurl (replace-regexp-in-string "20080" "20090" tmpurl))
					
					;; lookup=....&optional=....
					(setq params (join-string (loop for ( key . val ) in query
																					collect (format "%s=%s"
																													(url-hexify-string (cond ((symbolp key) (symbol-to-string key))
																																									 (t key)))
																													(url-hexify-string (cond ((vectorp val) (json-encode val))
																																									 ((listp val)   (json-encode val))
																																									 ((numberp val) (number-to-string val))
																																									 ((symbolp val) (symbol-name val))
																																									 (t val))))
																					) "&" ))
					;;
					;; url, url-request-method, url-request-data, url-request-extra-headers を設定して、(url-retrieve-synchronously url) を実行するとbufferにresponseが返る。 (url.el)
					;;
					(destructuring-bind ( url-request-method . url )
							(case action
								;; (:new     (cons "GET"    (concat tmpurl "/new.howm"  "?" params )))
								;; (:edit    (cons "GET"    (concat tmpurl "/edit.howm" "?" params )))
								(:create  (cons "POST"   (concat tmpurl ".howm"       "?" params )))
								(:show    (cons "GET"    (concat tmpurl ".howm"       "?" params )))
								(:update  (cons "PUT"    (concat tmpurl ".howm"       "?" params ))) ;; createもupdateで処理してる?
								(:destroy (cons "DELETE" (concat tmpurl ".howm"       "?" params )))
								(:index   (cons "GET"    (concat tmpurl "/index.howm" "?" params ))))
						(setq url-request-data           (url-hexify-string (encode-coding-string (or body "") 'utf-8-unix)))
						(setq url-request-extra-headers `(("Content-Type"   . "text/howm")
																							("Content-Length" . ,(format "%d" (+ 2 (length url-request-data)))))) ;; url packageが "\r\n" を追加しているので +2 している。
						
						(save-excursion (condition-case nil
																(let (response response-header)
																	(when (get-buffer tt3-http-response-buffer) (kill-buffer tt3-http-response-buffer))
																	(set-buffer (generate-new-buffer tt3-http-response-buffer))
																	(insert (decode-coding-string 
																					 (with-current-buffer (url-retrieve-synchronously url) (buffer-string)) ;; ← http通信でテキストを得る
																					 'utf-8-unix))
																	
																	;; header部 を切り出しalist化し、response-headerとして返す。　バッファー(tt3-http-response-buffer)にはbody部が返る。
																	(goto-char (point-min)) (re-search-forward "\n\n") ;; 最初の改行x2がheader-body境界
																	(setq response-header (split-string (buffer-substring (point) (point-min)) "\n"))
																	(delete-region (point) (point-min))
																	(setq response (cons (cons "Buffer" (current-buffer))                           ;; ("Buffer" . "buffer-name")
																											 (loop for lin in response-header
																														 if (string-match "\\(: \\|HTTP\\)" lin)
																														 collect (let ((tmp (split-string lin "\\(: \\)")))
																																			 (case (length tmp)
																																				 (1 (cons "Status" (car tmp)))            ;; ("Status" . "" )
																																				 (2 (cons (car tmp) (cadr tmp))))))))
																	response)
															(error "error" '(("Status" . "error"))))))))




(provide 'tt3-resource)

