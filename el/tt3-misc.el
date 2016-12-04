;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-misc.el  thinktank


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; Thinktank 系コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

(defun tt:misc-copy-memoid () (interactive) (push-string-to-clipboard (thinktank3-format :memofile (buffer-name))))
(defun tt:misc-copy-memotitle () (interactive) (save-excursion (goto-char (point-min)) (push-string-to-clipboard (current-line-string))))

(defun tt3-misc-howmidp ( str ) (and (string-match "\\([0-9][0-9][0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9][0-9][0-9][0-9][0-9]\\)\\.howm" str) (match-string 1 str)))

(defun* tt:misc-clipboard-memoinfo ( &optional (info-type :id) ) (interactive)
				(push-string-to-clipboard 
				 (case info-type
					 (:id      (thinktank3-format :memoid (buffer-name)))
					 (:howm    (push-string-to-clipboard (thinktank3-format :memofile (buffer-name))))
					 (:title   (push-string-to-clipboard (first-line-string)))                                                       ; serverから得るか？
					 (:content (push-string-to-clipboard (buffer-string)))                                                           ; serverから得るか？
					 (:url     (tt3-resource-request-http :test :url :ext "html" :memoid (thinktank3-format :memoid (buffer-name))))
					 (:nodes ))))                                                                                                     ; serverから得るか？

	
;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; emacs編集系コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun tt3-misc-insert-timestamp ( timestamp-type insert-type )
	(let ((ts (case timestamp-type
							(:tt    (format-time-string "%Y-%m-%d-%H%M%S"))
							(:now   (format-time-string "%Y-%m-%d %a %H:%M"))
							(:today (format-time-string "%Y-%m-%d"))
							(:kyou  (format "%s年%s月%s日" (format-time-string "%Y") (format-time-string "%m") (format-time-string "%d"))))))
		(insert (case insert-type
							(:text ts)
							(:tag  (format "[%s]" ts))
							(:node (format "\n* [%s]" ts))))))

(defun* tt:misc-insert-today ( &optional (insert-type :today) ) (interactive) (tt3-misc-insert-timestamp :today insert-type))
(defun* tt:misc-insert-kyou  ( &optional (insert-type :today) ) (interactive) (tt3-misc-insert-timestamp :kyou  insert-type))
(defun* tt:misc-insert-now   ( &optional (insert-type :today) ) (interactive) (tt3-misc-insert-timestamp :now   insert-type))
(defun* tt:misc-insert-tt    ( &optional (insert-type :today) ) (interactive) (tt3-misc-insert-timestamp :tt    insert-type))


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; emacs制御系コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;; button-lock
(defun tt-button-lock-match-following-text ( regexp )
	(when (and (<= (car (button-lock-find-extent)) (point))
						 (<  (point) (cadr (button-lock-find-extent))))
		(goto-char (cadr (button-lock-find-extent)))
		(looking-at regexp)))

;; visual
(defun tt:misc-toggle-toolbar () (interactive) (tool-bar-mode (if tool-bar-mode 0 1)))
(defun tt:misc-toggle-scrollbar () (interactive) (scroll-bar-mode (if scroll-bar-mode 0 1)))
(defun tt:toggle-menubar () (interactive)   (menu-bar-mode (if menu-bar-mode 0 1)))
(defun tt:toggle-expand () (interactive) (tt:misc-toggle-toolbar) (tt:misc-toggle-scrollbar) (tt:toggle-menubar) (tt:toggle-full))
(defun tt:indent-all () (interactive) (indent-region (point-min) (point-max)))
(defun tt:set-alpha ( &optional alpha )	(interactive) (set-frame-parameter nil 'alpha (or alpha (read-number "Input ratio: " 100))))

;; edit
(defun tt:kill-word () (interactive) (kill-new (thing-at-point 'word)))
(defun tt:kill-read-string () (interactive) (kill-new (read-string "keyword:")))
(defun tt:search-forward-focused-string () (interactive)
	(let ((key (focused-string)))
		(when (setq key (read-string "search:" key))
			(search-forward key nil t))))

(defun tt:toggle-linum () (interactive)
	(set-face-attribute 'linum nil :foreground "#bf616a" :height 0.9)
	(setq linum-format "%4d")
	(linum-mode (if linum-mode 0 1)))

(defvar tt3-emacs-toggle-full nil)
(defun tt:toggle-full () (interactive)
	(case (window-system)
		('w32 (setq tt3-emacs-toggle-full (null tt3-emacs-toggle-full))
					(if tt3-emacs-toggle-full
							(w32-send-sys-command #xf030)
						(w32-send-sys-command 61728)))
		('ns (set-frame-parameter
					nil 'fullscreen
					(when (not (frame-parameter nil 'fullscreen)) 'maximized)))   ; fullboth)))
		('x  (set-frame-parameter nil 'fullscreen (if (frame-parameter (selected-frame) 'fullscreen) nil 'fullboth)))))


(defun tt:truncate-lines () (interactive)
	(setq truncate-partial-width-windows nil)
  ; (toggle-truncate-lines)
	(setq truncate-lines (if truncate-lines nil t))
	(redraw-frame))

(defun thinktank-switch-ime ( sw )
	(case (window-system)
		('w32 (if sw (ime-force-on) (ime-force-off)))
		('ns  (if sw (toggle-input-method) (inactivate-input-method))))) ;; (toggle-input-method)


(defvar thinktank-hilight-text "")
(defun* tt:highlight-word ( &key keyword regexp ) (interactive)
	(tt:unhilight)
	(setq thinktank-hilight-text (cond (keyword           (regexp-quote keyword))
																						(regexp            regexp)
																						((region-active-p) (regexp-quote (buffer-substring (region-beginning) (region-end))))
																						(t                 (regexp-quote (read-string "word:" (thing-at-point 'word))))))
	(highlight-regexp thinktank-hilight-text))

(defun tt:unhilight () (interactive)
	(unhighlight-regexp thinktank-hilight-text)
	(setq thinktank-hilight-text nil))









(provide 'tt3-misc)


