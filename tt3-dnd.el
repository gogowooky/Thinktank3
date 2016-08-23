;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-dnd.el  thinktank

;; ファイル名内部に '[', ']' があれば削除するように促す。
;;
;; 上のorg-linkで作用させる以下の関数を作成する
;; link-to-combine
;; copy-linked-file-to
;; copy-combined-file-to
;; indirectlink-to-directlink
;; directlink-to-indirectlink


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun* thinktank3-dnd-insert-tag ( filepath &optional arg ) (interactive) "
* [説明] thinktankメモへのdrag and dropを処理する。
  [引数] filepath :  ファイル
  [用例]
     dnd             : ファイルopen
     C-u dnd         : ファイルlink挿入
     C-u C-u dnd     : ファイルをメモdirectoryにコピーして、そのlinkとoriginal linkを挿入
     C-u C-u C-u dnd : ファイルをメモdirectoryにコピーして、そのlinkを挿入

　[2014-02-02 Sun] 修飾キー付きのdndはmacでは不可なので、C-u prefix 付きで対処することにした
"
				(cond ((null arg)                         ;; open
							 (condition-case nil (org-open-file filepath) (error (find-file filepath))))
							
							((equal arg '(4))                   ;; link ( C-u dnd )
							 (tt3-dnd-insert-link-tag filepath))

							((equal arg '(16))                  ;; combine ( C-u C-u dnd )
							 (tt3-dnd-insert-link-tag filepath)
							 (tt3-dnd-link-to-combine :with-orig))

							((equal arg '(64))                  ;; combine ( C-u C-u C-u dnd )
							 (tt3-dnd-insert-link-tag filepath)
							 (tt3-dnd-link-to-combine :no-orig))))

(defun thinktank3-dnd-link-to-combine () (interactive)
	;;
	;; コピー先にファイルがある場合は変換しない。
	;;
	(tt3-dnd-link-to-combine))

(defmacro thinktank3-dnd-initialize () ;; tt-mode.el で使用
	'(case (window-system)
		 ('w32 (define-key map [drag-n-drop]  'tt3-dnd-file-w32))
		 ('ns  (define-key map [ns-drag-file] 'tt3-dnd-file-ns))))


(defun thinktank3-dnd-downloaded-file () (interactive)
	'((setq x-select-enable-clipboard t)
		(tt3-dnd-insert-link-tag filepath)
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CURD / CONTROL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tt3-dnd-file-w32 () (interactive)
	(mapc '(lambda (filepath) (thinktank3-dnd-insert-tag filepath current-prefix-arg)) 
				(nth 2 last-input-event)))

(defun tt3-dnd-file-ns () (interactive)
	(when ns-input-file
		(mapc '(lambda (filepath) (thinktank3-dnd-insert-tag filepath current-prefix-arg))
					ns-input-file))
	(setq ns-input-file nil))


(defvar tt3-dnd-config-env-dir
	;;
	;; thinktank3-config内の、( :ttxxxx-dir . "(directory)/" ) を抽出 ( "xxxx" . "(directory)" ) に改変して登録
	;;
	;; process-environment に 環境変数として登録。　file:$tttmp/test.txt 等が使用できるようになる。
	;;
	(delete nil (mapcar (lambda (x)	
												(let* ((conf-car (symbol-name (car x)))
															 (conf-cdr (cdr x))
															 env-var env-dir)
													(when (and (string-match ".*dir$" conf-car) (stringp conf-cdr) (file-exists-p conf-cdr))
														(setq env-var (replace-regexp-in-string ":\\(.*\\)" "\\1" conf-car))
														(setq env-dir (replace-regexp-in-string "\\(.*\\)/" "\\1" conf-cdr))
														(push (format "%s=%s" env-var env-dir) process-environment) ; ついでにprocess-environmentも設定
														(cons env-var env-dir)

														)))
											tt3-config-data-table)))


(defun tt3-dnd-insert-combine-tag-for-download ()
	(let* ((filedir  (thinktank3-format :memodir (current-buffer)))

				 (finename (format "%s.pdf" (thinktank3-format :memoid :now)))
				 )
		(insert (format "[[tt:/%s]]" filename))
		(save-excursion 
			(set-buffer (get-buffer-create "tmp"))
			(insert filepath
							))))

(defun tt3-dnd-insert-link-tag ( filepath )
	;;
	;; ファイルへのlinkを作成する。
	;;
	;; linked-file :: [[file:/(filepath)][(filename) on PCNAME/tttmp/ttsync/dropbox]]
	;;
	(when (string-match "[\]\[]" filepath)
		(let ((newname (replace-regexp-in-string "[\]\[]" "_" filepath)))
			(rename-file filepath newname)
			(setq filepath newname)))
	
	(let* ((curpos (point))
				 (filename (file-name-nondirectory filepath))
				 (filedir  (file-name-directory filepath)))
		
		(setq filepath (concat filedir filename)) ;; separatorをbackslashに
		
		(when (loop for (env-var . env-dir) in tt3-dnd-config-env-dir
								if  (string-match (regexp-quote env-dir) filepath)
								do  (cond ((string-match env-var "ttmemo")                                              ;; tt:xxx/
													 (insert (format "[[tt:%s/%s]]\n" (thinktank3-format :memoid filedir) filename)))
													(t                                                                            ;; file: 間接参照
													 (insert (format "[[file:%s][%s:%s]]\n"
																					 (replace-regexp-in-string (regexp-quote env-dir) (concat "$" env-var) filepath)
																					 env-var
																					 filename))))
								never (string-match (regexp-quote env-dir) filepath))
			
			(insert (format "[[file:%s][%s:%s]]\n" filepath system-name (file-name-nondirectory filepath)))) ;; file: 間接参照無し
		(set-mark (point))
		(goto-char curpos)))


(defun tt3-dnd-link-to-combine ( mode )
	;;
	;; linkファイルをmemo-dirにコピー(=combine)して、org-tagを書き換える。
	;;
	;; combined-file :: [tt:/(filename)] ([[file:/(filepath)][original on PCNAME/tttmp/ttsync/dropbox]])
	;;
	(narrow-to-region (line-beginning-position) (line-end-position))
	(beginning-of-line)

	(if (re-search-forward "\\[\\[file:\\(.*\\)\\]\\[\\(.*\\):\\(.*\\)\\]\\]")
			(let ((filepath (match-string 1))
						(location (match-string 2))
						(filename (match-string 3))
						(memodir  (file-name-directory (thinktank3-format :memodir (buffer-name)))))
				
				(condition-case nil (progn
															(setq filepath (substitute-in-file-name filepath))
															(copy-file filepath (concat memodir filename))
															(re-search-backward "\\[\\[file:\\(.*\\)\\]\\[\\(.*\\):\\(.*\\)\\]\\]")
															(case mode
																(:with-orig (replace-match (format "[[tt:/%s]] ([[file:%s][orig on %s]])" filename filepath location)))
																(:no-orig   (replace-match (format "[[tt:/%s][%s]]" filename (file-name-extension filename))))))
					(error nil))))
	(widen))





(provide 'tt3-dnd)
