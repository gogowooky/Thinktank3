;; tt-system.l; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'url)
(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (thinktank3-config   &optional key value )
;; (thinktank3-format   &optional param )
;; (thinktank3-property &optional node-address key value )
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar tt3-config-data-table   nil)  ;; システム設定値をproperty-listで保持する。　
(defvar tt3-property-data-table nil)  ;; 全システムメモをnode単位に分解、( "title" pos "howm filename" ) のlistとして全nodeを保持する。


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; thinktank3-config: 
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;;
;; システム設定値を返す。
;;
(defun* thinktank3-config ( &optional key value ) "
* [説明] system起動に必要な propertyを thinktank起動時に読み取りconfig値として保持している。
         nest無しの単純alist。
  [用例]
    (thinktank3-config)
    (thinktank3-config :memodir)
    (thinktank3-config :memodir \"C:/\")
    (thinktank3-config :delete-key :memodir) "

				(cond ((null key)           tt3-config-data-table)
							((eq key :set-memodir)
							 (push (cons :memodir (with-temp-buffer (insert-file-contents (concat (file-name-directory (locate-library "tt.el"))
																																										"configuration/memodir@" (upcase (system-name))
																																										".conf"))
																											(current-line-string))) tt3-config-data-table))
							((eq key :initialize)
							 (push (cons :tempdir          (thinktank3-property "Thinktank.Host.thinktank" "tempdir")) tt3-config-data-table)
							 (push (cons :syncdir          (thinktank3-property "Thinktank.Host.thinktank" "syncdir")) tt3-config-data-table)
							 (push (cons :baseurl          (thinktank3-property "Thinktank.Host.thinktank" "url")) tt3-config-data-table)
							 (push (cons :firefox          (thinktank3-property "Thinktank.Host.thinktank" "firefox")) tt3-config-data-table)
							 (push (cons :template-memo    (thinktank3-property "Thinktank.Template" "memo")) tt3-config-data-table)
							 (push (cons :template-oneline (thinktank3-property "Thinktank.Template" "oneline")) tt3-config-data-table)
							 (push (cons :memo             (concat (thinktank3-property "Thinktank.Host.thinktank" "url") "memo")) tt3-config-data-table)
							 (push (cons :memos            (concat (thinktank3-property "Thinktank.Host.thinktank" "url") "memos")) tt3-config-data-table))

							((eq key :initialize2) (setq tt3-config-data-table `((:memodir . ,tt3-memodir)  ;  (thinktank3-property "Thinktank.Host.thinktank" "memodir")
																																	(:tempdir . ,(thinktank3-property "Thinktank.Host.thinktank" "tempdir"))
																																	(:syncdir . ,(thinktank3-property "Thinktank.Host.thinktank" "syncdir"))
																																	(:baseurl . ,(thinktank3-property "Thinktank.Host.thinktank" "url"))
																																	(:firefox . ,(thinktank3-property "Thinktank.Host.thinktank" "firefox"))
																																	(:template-memo    . ,(thinktank3-property "Thinktank.Template" "memo"))
																																	(:template-oneline . ,(thinktank3-property "Thinktank.Template" "oneline"))
																																	(:memo    . ,(concat (thinktank3-property "Thinktank.Host.thinktank" "url") "memo"))
																																	(:memos   . ,(concat (thinktank3-property "Thinktank.Host.thinktank" "url") "memos")))))
							((null value)         (assoc-default key tt3-config-data-table))
							((eq key :delete-key) (setq tt3-config-data-table (delete* value tt3-config-data-table :test (lambda ( x y ) (eq x (car y))))))
							(t                    (thinktank3-config :delete-key key) 
																		(push (cons key value) tt3-config-data-table))))


;(defvar tt3-memodir ;; memodirは memodir@MACHINE-NAME.conf ファイルの一行目に書かれているものを用いる。
;	(with-temp-buffer
;		(insert-file-contents (concat (file-name-directory load-file-name)
;																	"configuration/memodir@" (upcase (system-name))
;																	".conf"))
;		(current-line-string)))



;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; thinktank3-format
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

(defun* thinktank3-format ( type &optional param ) "
* [説明] thinktank関連のID文字列を作成する。
  [引数]
    type     :memoid, :memofile, :memodir, :memopath, :memotitle, :logfile, :logpath, :tmpdir, :logpath-wc
    memoid   :now, xxxx-xx-xx-xxxxxx(を含む文字列), :home, nil
  [用例]
    (thinktank3-format :tmpdir)
    (thinktank3-format :rememoid \"0000-00-00-000000\")
    (thinktank3-format :memopath \"0000-00-00-000001.dic\")
    (thinktank3-format :logpath  \"1999-08-01-000000\") "

				(let (year month day hms memodir logdir logid tmpdir baseurl memoid)
					(setq memoid param)
					(setq tmpdir (convert-standard-filename (thinktank3-config :tempdir)))
					(case type
						(:tmpdir  tmpdir)
						(t
						 (when (setq memoid (cond ((equal   memoid :now) (format-time-string "%Y-%m-%d-%H%M%S"))
																			((assoc-default memoid (thinktank3-config :memoalias)))
																			((stringp memoid)      (and (string-match "[0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]" memoid)
																																	(match-string 0 memoid)))))
							 (setq year  (substring memoid 0 4))
							 (setq month (substring memoid 5 7))
							 (setq day   (substring memoid 8 10))
							 (setq hms   (substring memoid 11))
							 (case type
								 (:rememoid (format "%s\\-%s\\-%s\\-%s" year month day hms))
								 (t
									(setq memodir (convert-standard-filename (concat (thinktank3-config :memodir) (if (string-match "0000" year) (format "%s/" memoid) (format "%s/%s/%s/" year month memoid)))))
									(setq logdir  (convert-standard-filename (concat (thinktank3-config :memodir) (unless (string-match "0000" year) (format "%s/%s/" year month)))))
									(case type
										(:memoid     memoid)
										(:memofile   (concat memoid ".howm"))
										(:memodir    memodir)
										(:memopath   (concat memodir memoid ".howm"))
										(:memolink   (cons (concat memoid ".howm") (and (string-match (concat memoid "\\.howm::\\([^ 　\t]+\\)") param) (match-string 1 param))))
										(:memotitle  (with-temp-buffer (ignore-errors (insert-file-contents (concat memodir memoid ".howm"))
																																	(buffer-substring (progn (beginning-of-buffer) (point)) (progn (end-of-line) (point))))))
										(:logfile    (concat memoid ".log"))
										(:logpath    (concat logdir memoid ".log"))
										(:logpath-wc (concat logdir (format "%s-%s-%s-??????.log" year month day)))))))))))




;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; thinktank3-property:  システムメモ(0000-00-00-00000?.howm)に記載したシステムプロパティを読み出す。
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

(defun thinktank3-property ( &optional node-address key value ) "
* [説明] node-addressで指定されるkeyの値を取得またはvalueに書き換える。
  [例]  (thinktank3-property \"Thinktank.Host.thinktank\" \"url\" )
"
	(cond ((equal node-address :initialize)  (tt3-property :initialize))  ;; 次回更新
				((equal node-address :buffer) (tt3-property :buffer))           ;; buffer取得
				((stringp node-address)       ;; (thinktank3-property "Thinktank.Host.thinktank" "url")
				 (cond ((equal key :keyword) 	 (last-tt3-property-node node-address  (tt3-tt3-property-get-element :keyword value)))             ;; keywordのvalueを返す ( #+KEY: VALUE の形式 )
							 ((equal key :src-block) (last-tt3-property-node node-address  (tt3-tt3-property-get-element :src-block)))                 ;; src-blockのvalueを返す ( #+begin_src のみ )
							 ((equal key :text)      (last-tt3-property-node node-address  (tt3-tt3-property-get-element :paragraph)))                 ;; paragraphを返す
							 ((equal key :json)      (json-read-from-string 
																				(last-tt3-property-node node-address (tt3-tt3-property-get-element :paragraph))))                ;; paragraphをjsonとして読み出す              
							 ((stringp key)          (let* (prop)
																				 (setq prop (last-tt3-property-node node-address (tt3-tt3-property-get-element :property key)))  ;; 以下propertyの編集
																				 (if (equal prop ":local")                                                  ;; valueが:localの場合、system memoではなくtmpdir/node-address.howmにある値を使う
																						 (cond ((equal value :delete) (tt3-property-local node-address key nil))          ;; key削除
																									 ((stringp value)       (tt3-property-local node-address key value))        ;; key値変更
																									 (t                     (tt3-property-local node-address key)))             ;; key値
																					 (cond ((equal value :delete)   (tt3-property-put-property node-address key nil))
																								 ((stringp value)         (tt3-property-put-property node-address key value))
																								 (t                       prop)))))
							 (t nil)))
				(t (tt3-property :values))                                                                                          ;; 全propertyを返す
				))


(defun* tt3-property ( &optional action ) "
* [説明] system memoをloadし、全propertyを読み込む。
         propertyは ( node-address  node-position  origin-file ) の list として保存されている。
         property値自体は buffer の node-position の nodeで読み取る
"
	(let ((tt3-property-buffer-name "*prop-buf3*"))  ;; このバッファー上に全system memoが読み込まれる。

		(case action
			(:values tt3-property-data-table)
			(:buffer (or (get-buffer tt3-property-buffer-name)
									 (progn (tt3-property :initialize) (get-buffer tt3-property-buffer-name))))

			(:initialize (ignore-errors (kill-buffer tt3-property-buffer-name))                               
									 ;; 全system file読み込む --------------------------------------------------------------------------------------------------------------------
									 (save-excursion
										 (set-buffer (generate-new-buffer tt3-property-buffer-name))
										 (loop for dir in (sort (directory-files (thinktank3-config :memodir)) 'string<)
													 for fil = (condition-case nil (concat (thinktank3-config :memodir) dir "/" (substring dir -17) ".howm") (error "_"))
													 if (file-exists-p fil)
													 do (progn (insert (format "\n* ======== %s.howm ========\n" (substring dir -17)))
																		 (insert-file-contents fil)
																		 (goto-char (point-max))))
										 
										 (org-mode) (show-all) (beginning-of-buffer)	(outline-next-heading) (sleep-for 0.1) ;; org処理の準備
										 
										 ;; リスト作成 ( node-address  node-position  origin-file ) --------------------------------------------------------------------------------
										 (flet ((get-node-heading-and-pos (lvl ttl orig) 
																											(loop for ( elemtype elemplist ) = (org-element-at-point) ;; (:headline (plist))
																														for elemlvl = (or (getf elemplist :level) 0)        ;; :headlineはplistに:levelを持つ
																														with pos = 0
																														while (and (<= lvl elemlvl) pos)
																														append (let* ((nodettl (replace-regexp-in-string (concat "@" (upcase (system-name)) "$") "" (getf elemplist :title))) ; 自環境nodeは@MACHINEを取る
																																					(elempos (getf elemplist :begin))
																																					(elemttl (concat ttl "." nodettl)))
																																		 '(msgbox "[%s][%s][%s][%s:%s][ori:%s]" nodettl elempos elemttl elemlvl lvl orig)
																																		 (setq pos (outline-next-heading))
																																		 (when (string-match "\\([0-9\-]\\{17\\}\\.howm\\)" nodettl) (setq orig (match-string 1 nodettl)))
																																		 (when (= lvl elemlvl) (cons (list (substring elemttl 1) elempos orig)
																																																 (get-node-heading-and-pos (+ 1 lvl) elemttl orig)))))))
											 ;; 最後のｱｲﾃﾑで抜け出せなくなる
											 (setq tt3-property-data-table (get-node-heading-and-pos 1 "" "")))))
			)))
		


;; 以下３つはlist内の指定nodeを回すマクロ
(defmacro mapcar-tt3-property-subnode ( parent-node-addr &rest body ) "
* [説明] parent-node-addr配下のsubnodeをnarrowingして巡回し、bodyを評価した値をlistで返すマクロ
         body内で以下の変数が使える
         title:            node addr   ex)Extension.Queries.Memo.All
         parent-node-addr: 親のアドレス "
	`(with-current-buffer (thinktank3-property :buffer)
		 (loop for ( title pos orig-filename ) in tt3-property-data-table
					 with regexp = ,(concat "^" (regexp-quote parent-node-addr) "\\.[a-zA-Z0-9_\\.\\-]+$")
					 if (string-match regexp title)
					 collect (unwind-protect (save-excursion (save-restriction (goto-char pos) (beginning-of-line) (org-narrow-to-subtree) ,@body)) (widen)))))


(defmacro last-tt3-property-node ( node-addr &rest body ) "
* [説明] mapcar-tt3-property-node でbodyを評価して得たlistの、最後の非nil値を返すマクロ"
	`(car (last (delq nil (mapcar-tt3-property-node ,node-addr ,@body)))))


(defmacro mapcar-tt3-property-node ( node-addr &rest body ) "
* [説明] node-addr指定に該当する複数nodeをnarrowingして巡回し、bodyで評価した値をlistで返すマクロ"
	
	`(with-current-buffer (thinktank3-property :buffer)
		 (loop for ( title pos orig-filename ) in tt3-property-data-table
					 with node-addr = ,node-addr
					 if (equal title node-addr)
					 collect (unwind-protect (save-excursion (save-restriction (goto-char pos) (beginning-of-line) (org-narrow-to-subtree) ,@body)) (widen)))))


;; 以下２つは上記マクロ内で動かす必要がある
(defun tt3-tt3-property-get-original-info ( elemtype &optional key ) "
* [説明] node-addrで指定されるnodeをnarrowingし、body評価が非nilの場合、( original-filename . pos )を返す "
	(when (and (tt3-tt3-property-get-element elemtype key) orig-filename pos) 
		(cons orig-filename (- pos 
													 (car (assoc-default (format "======== %s ========" orig-filename) tt3-property-data-table))
													 42))))

(defun tt3-tt3-property-get-element ( elemtype &optional key ) "
* [説明] カーソル位置のnodeでelemtype型の値を返す(マクロ専用version) "
	(case elemtype
		(:node      t)
		(:headline  (loop for (typ plis) = (org-element-at-point)
											while (= 0 (forward-line))
											if (equal typ 'headline)
											return (getf plis :raw-value)))
		(:property  (org-entry-get nil key))
		(:keyword   (loop for (typ plis) = (org-element-at-point)
											while (= 0 (forward-line))
											if (and (equal typ 'keyword) (equal key (getf plis :key)))
											return (getf plis :value)))
		(:src-block  (loop for (typ plis) = (org-element-at-point)
											 while (= 0 (forward-line))
											 if (and (equal typ 'src-block) (find (getf plis :language) '( "thinktank" "mozrepl") :test 'equal))
											 return (getf plis :value)))
		(:paragraph (loop for (typ plis) = (org-element-at-point)
											for lin = (current-line-string)
											with text = ""
											while (= 0 (forward-line))
											if (equal typ 'paragraph)
											do (setq text (concat text lin "\n"))
											finally return (trim-string text)))))

(defun tt3-tt3-property-get-element-general-use ( elemtype &optional key ) "
* [説明] 現カーソル位置のnodeでelemtype型の値を返す(マクロ非専用version) " 
	(unwind-protect
			(save-excursion (save-restriction (org-narrow-to-subtree)
																				(goto-char (point-min))
																				(beginning-of-line)
																				(tt3-tt3-property-get-element elemtype key)))
		(widen)))

(defun tt3-tt3-property-get-node-addr ()
	(save-excursion (save-restriction
										(let (headings)	(condition-case nil
																				(while t
																					(destructuring-bind (typ plis) (org-element-at-point)
																						(when (equal typ 'headline) (push (getf plis :raw-value) headings))
																						(org-up-element)))
																			(error (mapconcat 'identity headings ".")))))))


;; propertyを書き換える。　書き換えられるのはpropertyのみ。　keywordやparagraphを書き換える方法は提供しない。
(defun tt3-property-put-property ( node-address key value ) "
* [説明] node-addrで指定されるnodeのkey propertyの値をvalueに書き換える。"

	(if (and (stringp key) (equal ":local" (thinktank3-property node-address key)))
			(tt3-property-local node-address key value)

		(destructuring-bind ( filename . pos ) (or (last-tt3-property-node node-address (tt3-tt3-property-get-original-info :property key)) ;; 既存key
																							 (last-tt3-property-node node-address (tt3-tt3-property-get-original-info :node)))        ;; 新規key
			(when pos
				(if (get-buffer filename)
						(with-current-buffer filename 
							(org-entry-put pos key value) (save-buffer))
					(save-excursion (find-file (thinktank3-format :memopath filename))
													(org-mode)
													(org-entry-put pos key value)
													(sleep-for 0.1)
													(set-buffer-modified-p nil)
													;;(save-buffer)
													(write-region (point-min) (point-max) filename)
													(sleep-for 0.1)
													(kill-buffer)))

				(tt3-property :initialize)))))


(defun tt3-property-local ( node-address key &optional value ) "
* [説明] system memo内value値が :local のとき、tmp-dirのnode-address.homファイル内の値が利用される。
　　　　 認証キー等のPC localな値の記録に使用される
　　　　 node-addrで指定されるkey値を取得または、valueに書き換える。"
	(let* ((local-prop-file (concat (thinktank3-config :tempdir) node-address ".howm")))
		(unless (file-exists-p local-prop-file) (with-temp-file local-prop-file (insert "* Thinktank Property\n")))
		(save-excursion (find-file local-prop-file)
										(org-mode)
										(prog1 (cond (value (prog1 (org-entry-put nil key value) (save-buffer) (sleep-for 0.1)))
																 (t     (progn (org-entry-get nil key))))
											(kill-buffer)))))


(provide 'tt3-system)

