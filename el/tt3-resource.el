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
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : (tt3-resource-request-http)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
																				;
																				; サーバーと直接通信、ＨＴＴＰレスポンスヘッダーを返す。
																				; 
(require 'url)
(require 'json)
(defun* tt3-resource-alist2params ( alist )
	;; lookup=....&optional=....
	(if alist (concat "?" (join-string (loop for ( key . val ) in alist
																					 collect (format "%s=%s"
																													 (url-hexify-string (cond ((symbolp key) (symbol-to-string key))
																																										(t key)))
																													 (url-hexify-string (cond ((stringp val) val)
																																										((vectorp val) (json-encode val))
																																										((listp val)   (json-encode val))
																																										((numberp val) (number-to-string val))
																																										((symbolp val) (symbol-name val))
																																										(t val))))
																					 ) "&" ))))


(defun* tt3-resource-request-http ( &key (action :read) (host "0.0.0.0") (port "20091") (resource "memos") (ext "howm") (memoid "0000-00-00-000000") (query nil) (body "") ) "
* [説明] thinktank serverにアクセスし、res.headerをalistで返す。　res.bodyはbufferに保存されている。
  [引数] action   :  :show, :create, :update, :destroy, :index
         memoid   :  \"xxxx-xx-xx-xxxxxx\"
         query    :  alist
         body     :  serverへの送信テキスト
  [返値] res.headerのalist(下記)
         bufferに response body が返る。

	;; [返値] (assoc-default \"Buffer\" (tt3-resource-request-http :memoid \"0000-00-00-000000\"))で値を取得

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
				(cond ((string= ext "html")
							 (browse-url (case action
														 (:read   (concat "http://" host ":" port "/thinktank/" resource "/" memoid ".html"      (tt3-resource-alist2params query)))
														 (:index  (concat "http://" host ":" port "/thinktank/" resource            ".html"      (tt3-resource-alist2params query)))
														 (:edit   (concat "http://" host ":" port "/thinktank/" resource "/" memoid "/edit.html" (tt3-resource-alist2params query))))))

							
							((string= ext "howm")
							 (let* ((response-buf "*http-res*")
											url-request-data 
											url-request-extra-headers)
								 ;;
								 ;; url, url-request-method, url-request-data, url-request-extra-headers を設定して、(url-retrieve-synchronously url) を実行するとbufferにresponseが返る。 (url.el)
								 ;;
								 (destructuring-bind ( url-request-method . url )
										 (case action
											 (:create  `("POST"   . ,(concat "http://" host ":" port "/thinktank/" resource            ".howm" (tt3-resource-alist2params query))))
											 (:read    `("GET"    . ,(concat "http://" host ":" port "/thinktank/" resource "/" memoid ".howm" (tt3-resource-alist2params query))))
											 (:update  `("PUT"    . ,(concat "http://" host ":" port "/thinktank/" resource "/" memoid ".howm" (tt3-resource-alist2params query))))
											 (:destroy `("DELETE" . ,(concat "http://" host ":" port "/thinktank/" resource "/" memoid ".howm" (tt3-resource-alist2params query))))
											 (:index   `("GET"    . ,(concat "http://" host ":" port "/thinktank/" resource            ".howm" (tt3-resource-alist2params query)))))
									 
									 ;; (setq url-request-data           (url-hexify-string (encode-coding-string (or body "") 'utf-8-unix)))
									 (setq url-request-data           (encode-coding-string (or body "") 'utf-8-unix))
									 (setq url-request-extra-headers `(("Content-Type"   . "text/howm")
																										 ("Content-Length" . ,(format "%d" (+ 2 (length url-request-data)))))) ; url packageは最後に不必要な改行2文字を加えてしまう
									 (save-excursion (condition-case nil
																			 (let (heading-lines)
																				 ;; serverと通信
																				 (force-generate-new-buffer response-buf)
																				 (insert (decode-coding-string (with-current-buffer (url-retrieve-synchronously url) (buffer-string)) 'utf-8-unix))
																				 
																				 ;; 最初の改行x2でheader/bodyに分割、headerをalist化して返す。　bodyはalistのbufferに残る。
																				 (goto-char (point-min)) (re-search-forward "\n\n")
																				 (setq heading-lines (split-string (buffer-substring (point) (point-min)) "\n"))
																				 (delete-region (point) (point-min))
																				 
																				 ;; alist化
																				 (cons (cons "Buffer" (current-buffer))                           ; ("Buffer" . "buffer-name")
																							 (loop for lin in heading-lines
																										 if (string-match "\\(: \\|HTTP\\)" lin)              
																										 collect (let ((tmp (split-string lin "\\(: \\)")))
																															 (case (length tmp)
																																 (1 (cons "Status" (car tmp)))            ; ("Status" . "HTTP/1.1 200 OK" )
																																 (2 (cons (car tmp) (cadr tmp))))))))     ; ("key" . "value" )
																		 (error "error" '(("Status" . "error"))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; コマンド : (tt:resource-start-server)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
																				;
																				; ＨＴＴＰサーバー起動
																				;

(defun tt3-set-shell-cmdproxy()	(interactive)
	(setq shell-file-name "cmdproxy" explicit-shell-file-name "cmdproxy")
	(setenv "SHELL" explicit-shell-file-name)
	(setq w32-quote-process-args t)
	(setq shell-command-switch "-c"))

(defun* tt:resource-start-server () (interactive) "* [説明] webrickを起動する。"
				(unless (string= "HTTP/1.1 200 OK " (assoc-default "Status" (tt3-resource-request-http)))  ;; local server未起動時に実行
					(setq default-directory (file-name-directory (substring (locate-library "tt.el") 0 -6))) 
					(case (window-system)
						('w32 (tt3-set-shell-cmdproxy)
									(with-temp-file (concat default-directory "thinktank.bat")
										(insert (format "%s:\n" (substring default-directory 0 1))
														(format "cd %s\n" default-directory)
														(format "ruby thinktank.rb --test run" ))
										(start-process-shell-command "thinktank-server" nil "start cmd.exe /k thinktank.bat")))
						('ns
						 (with-temp-file (concat default-directory "thinktank.applescript")
							 (setq default-directory (file-name-directory default-directory))
							 (insert "tell application \"Terminal\"\n"
											 "do script \"echo dummy\"\n"
											 (format "do script \"cd %s\" in window 1\n" default-directory)
											 "do script \"ruby thinktank.rb --test run\" in window 1\n"
											 "end tell\n"))
						 (cd (file-name-directory default-directory))
						 (shell-command (format "/usr/bin/osascript %sthinktank.applescript" default-directory)))
						('x 
						 (setq default-directory (file-name-directory (locate-library "tt.el"))) 
						 (start-process-shell-command "xfce" nil (format "xfce4-terminal --working-directory='%s' -H --command='ruby thinktank.rb --test run'" default-directory))))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : (tt3-resource-read-memo)
;; コマンド : (tt:resource-open-memo)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* tt:resource-open-memo ( &key memoid ) "メモを開く"
				(interactive)
				(cond
				 ;; 指定無 → 0000-00-00-000000.howm
				 ((null memoid) (tt3-resource-read-memo))
				 
				 ;; button-lock → xxxx-xx-xx-xxxxxx.howm + option
				 ((tt3-misc-howmidp memoid) (let* ((id (match-string 1 memoid)))
																			(if (tt-button-lock-match-following-text ":\\([^:\n]+\\)?\\(\\(:\\([^:\n]+\\)?\\)\\(:\\([^:\n]+\\)\\)?\\)?:" )
																					(let* ((1st (match-string 1))
																								 (2nd (match-string 4))
																								 (hl  (match-string 6))
																								 (jump (if 2nd (list (string-to-integer 1st) (string-to-integer 2nd)) (string-to-integer 1st))))
																						(tt3-resource-read-memo :memoid id :jump jump :highlight hl))
																				(tt3-resource-read-memo :memoid id))))))



(defun* tt3-resource-read-memo ( &key (memoid "0000-00-00-000000") (highlight :none) (jump :beginning) (to :buffer) ) 
	;; (tt3-resource-read-memo :to :string)                       ; 内容を文字列で返す
	;; (tt3-resource-read-memo :jump 11)                          ; 指定行
	;; (tt3-resource-read-memo :jump "WordPress")                 ; 文字検索
	;; (tt3-resource-read-memo :jump '(11 10))                    ; (指定行 指定文字数)
	;; (tt3-resource-read-memo :jump '(2 "wordpress"))            ; (指定行 文字検索)
	;; (tt3-resource-read-memo :jump '("WordPress" 2))            ; (文字検索 指定行)
	;; (tt3-resource-read-memo :jump '("WordPress" "wordpress")   ; (タイトル文字検索 文字検索)
	;; (tt3-resource-read-memo :highlight "mode")                 ; ハイライト文字

	"基本関数(1/5)"
	(interactive)
	(let ((header  (tt3-resource-request-http :action :read :memoid memoid)))
		(case to
			(:buffer (let ((bufname (concat memoid ".howm"))) ; バッファー名：　"xxxx-xx-xx-xxxxxx.howm"
								 (if (get-buffer bufname)
										 (switch-to-buffer bufname)
									 (progn
										 (switch-to-buffer (assoc-default "Buffer" header))
										 (rename-buffer bufname)
										 (thinktank-minor-mode 1)))
								 (tt3-resource-read-memo--setup-rawmemo :highlight highlight :jump jump)))
					
			(:string (save-excursion (set-buffer (assoc-default "Buffer" header)) (buffer-string)))

			(t 			 "error" ))))

(defun* tt3-resource-read-memo--setup-rawmemo ( &key highlight jump )
	(setq case-fold-search nil)
	(cond ((eq jump :beginning) (goto-char (point-min)))
				((eq jump :end)       (goto-char (point-max)))
	      ((numberp jump)       (goto-line jump))
				((listp jump)   (cond ((numberp (car jump))
															 (goto-line (car jump))
															 (cond ((numberp (cadr jump)) (beginning-of-line) (forward-char (1- (cadr jump))))
																		 ((stringp (cadr jump)) (search-forward (cadr jump) nil t) (backward-char (length (cadr jump))))))
															((stringp (car jump))
															 (when (re-search-forward (format "\\*+ .*\\(%s\\)" (car jump)) nil t) (goto-char (match-beginning 1)))
															 (cond ((numberp (cadr jump)) (next-line (cadr jump)))
																		 ((stringp (cadr jump)) (search-forward (cadr jump) nil t))))))
				((stringp jump) (search-forward jump nil t) (backward-char (length jump))))
	
	(cond ((stringp highlight)
				 (tt:highlight-word :keyword highlight))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : 
;; コマンド : tt:resource-browse-memo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* tt:resource-browse-memo ( &key memoid ) (interactive) (tt3-resource-request-http :memoid (if memoid memoid (file-name-base (buffer-name))) :ext "html"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : (tt3-resource-create-memo)
;; コマンド : tt:resource-new-memo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* tt:resource-open-new-memo           () (interactive) (tt3-resource-read-memo :memoid (tt:resource-new-memo :memotag :new) :jump :end))
(defun* tt:resource-link-new-memo      () (interactive) (insert (format "%s.howm" (tt:resource-new-memo :memotag :new))) (backward-char 22))
(defun* tt:resource-link-new-clipboard-memo () (interactive) (insert (format "%s.howm" (tt:resource-new-memo :memotag :clipboard))) (backward-char 22))
(defun* tt:resource-link-new-region-memo    () (interactive) (insert (format "%s.howm" (tt:resource-new-memo :memotag :move-region))) (backward-char 22))

(defun* tt:resource-new-memo ( &key memotag ) "新規メモを作成して開く" 
				(interactive)
				(let ((memoid (tt3-resource-create-memo)) (body ""))
					(case memotag
						
						;; 新規メモ作成
						(:new (setq body "\n"))
						
						;; クリップボードで新規メモ
						(:clipboard (setq body (with-temp-buffer (clipboard-yank) (buffer-string))))

						;; 選択範囲カットして新規メモ
						(:move-region (setq body (buffer-substring (region-beginning) (region-end)))
													(delete-region (region-beginning) (region-end)))

						;; 選択範囲コピーして新規メモ
						(:copy-region (setq body (buffer-substring (region-beginning) (region-end))))

						(t 
						 ;; button-lock → new.howm
						 (when (string= "new.howm" memotag) 
							 (apply 'delete-region (button-lock-find-extent))
							 (insert (format "%s.howm" memoid))
							 (tt3-resource-read-memo :memoid memoid))))
					
					(when (find memotag '(:new :clipboard :move-region :copy-region)) 
						(tt3-resource-request-http :action :update :memoid memoid :body (format "* NEW MEMO\n** %s\n%s" (format-time-string "[%Y-%m-%d]") body)))
					
					memoid ))
					
							 


(defun* tt3-resource-create-memo () 
	"基本関数(2/5) memoidを返す"
	(interactive)
	(save-excursion	(set-buffer (assoc-default "Buffer" (tt3-resource-request-http :action :create)))	(buffer-string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : tt3-resource-delete-memo
;; コマンド : tt:resource-delete-memo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* tt:resource-delete-memo ( &key memoid )	"メモを削除" 
				(interactive)
				(cond
				 ;; 現メモ削除
				 ((null memoid)	(let* ((id (tt3-misc-howmidp (buffer-name)))) (when id (tt3-resource-delete-memo :memoid id) (kill-buffer))))
				 
				 ))


(defun* tt3-resource-delete-memo ( &key memoid) 
	"基本関数(3/5)"
	(interactive) 
	(save-excursion	(set-buffer (assoc-default "Buffer" (tt3-resource-request-http :action :destroy :memoid memoid))) (buffer-string)))
																				;
																				; (tt3-resource-delete-memo :memoid (tt3-resource-create-memo))
																				;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : tt3-resource-update-memo
;; コマンド : tt:resource-update-memo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* tt:resource-update-memo ( &key memoid body ) "メモ更新" 
				(interactive)
				(cond
				 ;; 現メモ更新
				 ((null memoid) (save-buffer))

				 ;; サーバー側でupdate
				 ((and memoid body) (tt3-resource-update-memo :memoid memoid :body body))

				 ))
																				; 
																				; '((let ((id (tt3-resource-create-memo)))
																				;   (tt3-resource-update-memo :memoid id :body "TEST:えが")
																				;   (tt3-resource-read-memo :memoid id :to :string)))
																				;

(defadvice save-buffer (around tt:resource-update-memo activate) "メモを保存"
	(cond ((and (string-match "howm$" (buffer-name)) (tt3-misc-howmidp (buffer-name)))
				 (message "saving howm file")
				 (tt:resource-update-memo :memoid (tt3-misc-howmidp (buffer-name)) :body (buffer-string)))

				(t ad-do-it)))
																				;
																				; save-bufferにhookしておくと (org-capture) 内部からのcallにも対応できる。
																				; ((ad-disable-advice 'save-buffer 'around 'tt:resource-update-memo) ; 解除
																				;



(defun* tt3-resource-update-memo ( &key (memoid nil) (body nil) ) 
	"基本関数(4/5)"
	(interactive)
	(tt3-resource-request-http :action :update :memoid memoid :body body))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ローカル関数 : (tt3-resource-index)
;; コマンド : (tt:resource-index)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* tt:resource-index-all ()    (tt:resource-index :type :all))
(defun* tt:resource-index-search () (tt:resource-index :type :search :keyword (read-string "keyword: ")))


(defun* tt:resource-index ( &key (type :all) (keyword nil)) 
	(interactive)
	(case type
		(:all    (tt3-resource-index :query `((:type . "all"))))
		(:search (tt3-resource-index :query `((:type . "search") (:keyword . ,keyword))))))
																				;
																				; url option化でnil→nullとなりThinktankMemos::index内でおかしくなる。避けるにはnil値を渡さないこと。160919
																				;


(defun* tt3-resource-index ( &key (query nil) (to :buffer) (highlight :none)) ;; query(alist) : ((:type . "all") (:page . "1") (:limit . "500") (:sort ."memoid") (:dir . "ascend")
	"基本関数(5/5)"
	(interactive)
	(let ((header (tt3-resource-request-http :action :index :query query)))
		(case to
			(:buffer (let ((bufname (format "%s.lhowm" (assoc-default :type query)))) ; バッファー名：　"all.lhowm" "recent.lhowm" "search.lhowm"
								 (when (get-buffer bufname) (kill-buffer bufname))
								 (switch-to-buffer (assoc-default "Buffer" header))
								 (rename-buffer bufname)
								 (thinktanklist-minor-mode 1)
								 (tt3-resource-read-memo--setup-rawmemo :highlight highlight)))

			(:string (save-excursion (set-buffer (assoc-default "Buffer" header)) (buffer-string)))

			(t "error"))))









;; 
;; メモを列挙する
;;
(defun tt:resource-index2 ( &rest plist ) 
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




(provide 'tt3-resource)

