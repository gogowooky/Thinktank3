;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt-org.el  thinktank
(require 'org-install)

(require 'ob)
(require 'ob-eval)
(require 'ob-ruby)
(require 'ob-sqlite)
(require 'ob-python)
(require 'ob-emacs-lisp)
(require 'ob-js)
(require 'ob-org)
(require 'ob-R)
(require 'ob-sh)


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; orgタグ関連
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;; 日付タグ
(defun tt:org-insert-new-node () (interactive) (insert (format-time-string "\n* [%Y-%m-%d %a] ")))
(defun tt:org-insert-current-time () (interactive) (insert (format-time-string "[%Y-%m-%d %a %H:%M] ")))

;; footnoteタグ
(defun thinktank3-org-jump-next-footnote () (interactive))
(defun thinktank3-org-jump-previous-footnote () (interactive)   (org-footnote-goto-previous-reference label))
(defun thinktank3-org-jump-footnote-definition () (interactive) (org-footnote-goto-definition label))
(defun thinktank3-org-new-footnote () (interactive)             (org-footnote-new))
(defun tt:org-open-footnote-file () (interactive) (tt3-org-open-footnote :link))
(defun tt:org-open-footnote-site () (interactive) (tt3-org-open-footnote :pubmed))

(defun* tt3-org-open-footnote (target)
	(let (fntag) (save-excursion
								 (setq fntag (let ((pos (point)))
															 (forward-char) (search-backward "[" nil t)
															 (when (and (looking-at "\\[fn:[^\]]+\\]") (< pos (match-end 0))) (match-string 0))))
								(when (and fntag (re-search-forward (concat "^" (regexp-quote fntag))))
									(case target
										(:link   (when (re-search-forward "\\[\\[\\([^\]]+\\)\\]\\[\\([^\]]+\\)\\]\\]") (command-execute (kbd "C-c C-o"))))
										(:pubmed (let ((pos (match-end 0)))
															 (narrow-to-region (line-beginning-position) (line-end-position))
															 (if (search-forward "[[" nil t) (forward-char -2) (end-of-line))
															 (widen)
															 (browse-url (format (assoc-default "pubmed" thinktank3-org-web-search-links)
																									 (url-hexify-string (buffer-substring pos (point))))))))))))


;; tt タグ
(org-add-link-type "tt" 'thinktank3-org-open-tt-tag)        ;; ex) [tt:0000-00-00-000000.howm?xxx=]

(defun thinktank3-org-open-tt-tag ( link )
	;;
	;; - ジャンプタグ
	;;   tt：xxxx-xx-xx-xxxxxx.howm
	;;   tt：xxxx-xx-xx-xxxxxx.howm？jump=(キーワード), tt：xxxx-xx-xx-xxxxxx.howm？(キーワード) 
	;;   tt：xxxx-xx-xx-xxxxxx.howm？node=ノード文字,   tt：xxxx-xx-xx-xxxxxx.howm？*(ノード文字)
	;;
	;; - ファイル操作
	;;   tt：xxxx-xx-xx-xxxxxx
	;;   tt：xxxx-xx-xx-xxxxxx？open=(ファイル名), tt：xxxx-xx-xx-xxxxxx？(ファイル名)
	;;
	;; - ジャンプタグ(current memo)  
	;;   tt：？jump=(キーワード), tt：？(キーワード)
	;;   tt：？node=(ノード文字)
	;;   tt：？dir=(ディレクトリ名)
	;;   tt：？file=(ファイル名)
	;;
	;; - 検索タグ
	;;   [未実装] tt：extension=
	;;   [未実装] tt：json=
	;;   [未実装] tt：target=obj,text=xxxx,
	;;   tt：(キーワード), tt：input=(キーワード)
	;;

	(if (string-match "\\([0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]\\.howm\\)?\\(.*\\)?" link)

		(let ((memoid (match-string 1 link))
					(params (loop for par in (split-string (match-string-no-properties 2 link) "[\?\&]" t)
												collect (split-string par "="))))

			(if memoid
					(tt3-resource-show-memo :memoid memoid)
				
				(let ()
					(cond ((= 1 (length (car params)))
								 (cond ((string-match "^#" (caar params)) ; #(キーワード)
												(tt3-resource-show-memo :memoid (buffer-name) :jump (substring 1 (caar params))))

											 (t ; (キーワード)
												(thinktank3-resource-index :name "Extension.Queries.Memo.Search" :input (caar params)))))

								((assoc-default "input" params)  ; input=(キーワード)
								 (thinktank3-resource-index :name "Extension.Queries.Memo.Search" :input (car (assoc-default "input" params))))

								((assoc-default "jump" params)  ; jump=(キーワード)
								 (tt3-resource-show-memo :memoid (buffer-name) :jump (car (assoc-default "jump" params))))

								(t 
								 (msgbox "not available"))))))

		(msgbox "not available")))
				 

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; org-babel: thinktank
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;; #+BEGIN_SRC thinktank ... #+END
(defvar org-babel-default-header-args:thinktank '((:results . "output silent") (:exports . "none" )))
(defun org-babel-execute:thinktank ( body params )
	(thinktank3-property :initialize)
	(thinktank3-resource-index :lookup body
														 :name   "thinktank"
														 ;; 同chapterのproperty値を引数にとる
														 :senddata (org-entry-get nil "senddata")
														 :cache    :no
														 :message  (org-entry-get nil "message")
														 :input    (org-entry-get nil "input")
														 :output   (org-entry-get nil "output")
														 :attrib   (org-entry-get nil "attrib")
														 :upper    (org-entry-get nil "upper")
														 :lower    (org-entry-get nil "lower")
														 ))

;; #+BEGIN_SRC mozrepl ... #+END
(defvar org-babel-default-header-args:mozrepl '((:results . "output silent") (:exports . "none" )))
(defun org-babel-execute:mozrepl ( body params )
	(tt3-mozrepl-open-firefox)
	(thinktank3-property :initialize)
	(tt3-mozrepl-request body))




 

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; org-mode用設定
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(setq org-drawers (cons "MEMO" org-drawers))

(global-font-lock-mode 1)

(setq org-startup-folded nil)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-export-html-inline-images	t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords '((type "INBOX(i)" "TODO(t)" "SOMEDAY(s)" "WAIT(w)"
																"DELEGATE(l)" "PROJECT(1)" "AREA(2)" "GOAL(3)" "VISION(4)" "PURPOSE(5)" "|" "DONE(d)" "CANCEL(c)" "POSTPONE(o)" "REFERENCE(r)")))
;;(setq org-todo-keyword-faces '(("SOMEDAY" . "green")))

(setq org-tag-alist '((:startgroup . nil) ("仕事" . ?j) ("生活" . ?l) ("System" . ?s) ("邂逅" . ?k) (:endgroup . nil)
											(:startgroup . nil) ("TBS" . ?t) (:endgroup . nil)))

(setq org-log-done 'time)

(push '("\\.xls\\(m\\|x\\)?\\'" . default) org-file-apps)
(push '("\\.pptx?\\'" . default) org-file-apps)
 


(provide 'tt3-org)
