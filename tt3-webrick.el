;; tt-webrick.l; -*- mode: emacs-lisp; coding: utf-8 -*-

(defun tt:start-webrick () (tt3-resource-start-webrick "version2.1"))

(defun tt3-resource-start-webrick ( &optional option ) (interactive) "
* [説明] webrickを起動する。
  [注意] win/macではscriptのあるディレクトリにバッチファイルを作成する。"
	(progn
		(setq default-directory (file-name-directory (locate-library "tt.el")))  '((locate-file))

		;; 起動中local serverがあれば立ち上げない。
		(unless (tt:resource-check-server)
		
			;; shell窓でrubyを立ち上げる
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
				
				('x (start-process-shell-command "xfce" nil (format "xfce4-terminal --working-directory='%s' -H --command='ruby thinktank.rb %s'" default-directory option)))))))



(provide 'tt3-webrick)

