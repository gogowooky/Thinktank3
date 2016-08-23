;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt-util.el  thinktank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 汎用
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; 文字列
;;
(defun* join-string ( string-list &optional (sep "\n") ) (mapconcat 'identity string-list sep))

(defun adjust-string ( str width )      "* [説明] 文字列をWIDTHの半角幅にそろえる" 
	(truncate-string-to-width str width nil ?  t))

(defun trim-string (text)               "* [説明] 複数行テキストの前後空白と空白行を削除する。"
	(when (stringp text)
		(mapconcat 'identity (delete-if (lambda (x) (equal "" x)) (mapcar (lambda (x) (replace-regexp-in-string "\\(^[ 　\t]*\\|[ 　\t]+$\\)" "" x)) (split-string text "\n"))) "\n")))

(defun symbol-to-string (sym)           "* [説明] symbolを文字列化する"	
	(replace-regexp-in-string ":" "" (symbol-name sym)))

;;
;; リスト
;;
(defun flatten (tree &optional stack)   "* [説明] listを平坦化する。"
	(if tree (if (atom tree) (cons tree stack)
						 (flatten (cdr tree) (flatten (car tree) stack)))
		(reverse stack)))


;;
;; editor
;;
(defun current-line-string ()           "* [説明] カーソル行の文字列を得る"
	(buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun current-select-string ()         "* [説明] カーソル直下の単語を得る"
	(when (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end))))

(defun focused-string ()                "* [説明] 選択文字またはカーソル直下の単語を得る"
	(or (current-select-string)	(thing-at-point 'word)))

(defun clipboarded-string ()            "* [説明] クリップボード文字列を得る"
	(with-temp-buffer (clipboard-yank) (buffer-string)))

(defun push-string-to-clipboard ( str ) "* [説明] クリップボードに指定文字列を格納する"
	(with-temp-buffer (insert str) (clipboard-kill-region (point-min) (point-max))))

;;============================================================================================================================================
;; atree ⇔ alist converter                    >> 2014-09-04-112911.howm, >> 2014-09-21-070916.howm
;;============================================================================================================================================
(defun get-item-from-tree ( index-list src-tree )
	(loop for id in index-list
				for alist   = src-tree then hitalst
				for hitalst = (car (assoc-default id alist))
				finally return (cons index-list (assoc-default id alist))))

(defun get-item-from-list ( index-list src-list ) (cons index-list (assoc-default index-list src-list 'equal)))

(defun item2bra ( item ) (if (cdar item) (list (list (pop (car item)) (item2bra item))) (list (cons (caar item) (cdr item)))))

(defun bra2item ( tree ) (loop for body = tree then (cadar body)
															 for ids = (append ids (list (caar body)))
															 if (equal (cadar body) :item) 
															 return (cons ids (cdar body))))

(defun add-item-to-tree ( item src-tree )
	(flet ((aitt (node tree)
							 (cond ((equal tree :item) node)
										 ((assoc-default (caar node) tree) 
											(let ((child-node (cond ((equal (cadar node) :item) (cdar node))
																							(t (list (aitt (cadar node) 
																														 (car (assoc-default (caar node) tree)))))))
														(newtree (delete-if (lambda (x) (equal (car x) (caar node))) tree)))
												(push (cons (caar node) child-node) newtree)))
										 (t (push (car node) tree)))))
		(aitt (item2bra item) (copy-tree src-tree))))

(defun list2tree ( item-list )
	(let (tree) (mapc (lambda (x) 
											(setq tree (add-item-to-tree x tree)))
										(copy-tree item-list)) tree))

(defun add-list-to-tree ( item-list src-tree )
	(loop for item in item-list
				for tree = (add-item-to-tree item src-tree) then (add-item-to-tree item tree)
				finally return tree))

(defun tree2list ( item-tree )
	(flet ((tree2branch (id trees) (mapcar (lambda (x) (when id (push id (car x))) x)
																				 (loop for tree in trees
																							 if (equal :item (cadr tree))
																							 collect (cons (list (car tree)) (cdr tree))
																							 else
																							 append (tree2branch (car tree) (cadr tree))))))
		(tree2branch nil item-tree)))

(defun add-tree-to-tree ( tree1 tree2 ) (add-list-to-tree (tree2list tree1) tree2))


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; 汎用システム系関数
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun open-directory-with-os-filemanager ( path ) "* [説明] OS毎のファイルマネージャーシステムでディレクトリを表示する。"
	(case (window-system) 
		('w32 (start-process-shell-command "open" nil "start" path ))
		('ns  (start-process-shell-command "open" nil "open" path ))))

(defun open-directory-with-os-shellwindow ( path ) "* [説明] OS毎のシェルウィンドウをpathをカレントにして表示する。"
	(case (window-system) 
		('w32 (start-process-shell-command "shell" nil "start" "cmd.exe" "/k" (format "\"%s&cd %s\"" (substring item 0 2) path)))
		; ('w32 (start-process-shell-command "shell" nil "cmd.exe" "/k" (format "runas /user:egashira7 \"cmd.exe /k %s&cd %s\"" (substring item 0 2) path)))
		('ns  (with-temp-file (concat default-directory "opendir.applescript")
						(insert "tell application \"Terminal\"\n")
						(insert "do script \"echo dummy\"\n")
						(insert (format "do script \"cd %s\" in window 1\n" path))
						(insert "end tell\n"))
					(start-process "open-dir" nil "osascript" "opendir.applescript"))))



(provide 'tt-util)


