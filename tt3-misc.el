;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-misc.el  thinktank


;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; Thinktank 系コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

(defun tt:misc-copy-memoid () (interactive) (push-string-to-clipboard (thinktank3-format :memofile (buffer-name))))
(defun tt:misc-copy-memotitle () (interactive) (save-excursion (goto-char (point-min)) (push-string-to-clipboard (current-line-string))))
(defun tt:misc-copy-memopath () (interactive) (push-string-to-clipboard (thinktank3-format :memopath (buffer-name))))

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; emacs 系コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

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


