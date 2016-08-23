;;  tt-menu.l; -*- mode: emacs-lisp; coding: utf-8 -*-
(require 'cl)

(require 'helm-config)
(require 'helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (thinktank3-menu-clear)
;; (thinktank3-menu-add items) 
;;
;; (tt:menu-show-tree-menu  &optional arg ) (interactive "P")
;; (tt:menu-show-list-menu  &optional arg ) (interactive "P")
;; (tt:menu-show-popup-menu &optional arg ) (interactive "P")
;; (tt:menu-initialize () (interactive)
;; (tt:menu-select-one-automatically) (interactive)
;; 
;; thinktank3-menu-before-initialize-hook
;; thinktank3-menu-after-initialize-hook
;;


;; [1/6] 各種メニュー表示コマンド
;; [4/6] helm自動選択
;; [5/6] コンテキスト記録
;; [6/6] メニュー表示関数
;;

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [1/6] 各種メニュー表示コマンド
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun tt:menu-show-tree-menu ( &optional arg )  (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-stringlist-menu tt3-menu-string-list :tree)))
(defun tt:menu-show-list-menu ( &optional arg )  (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-stringlist-menu tt3-menu-string-list :list)))
(defun tt:menu-show-popup-menu ( &optional arg ) (interactive "P") (with-tt3-menu-context arg (tt3-menu-show-stringlist-menu tt3-menu-string-list :popup))) '((tt:menu-show-popup-menu))

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [2/6] メニューの初期化
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defvar tt3-menu-default-string-list 
	'(( "T|Test 1|Test1" :func (msgbox "%s" (format-time-string "%Y-%m-%d %H:%M")) :help "テスト1")
		( "T|Test 2|Test2" :func (msgbox "%s" input) :input :key :help "テスト2")
		( "H|Help D|Document H|Help" :command help :help "ヘルプ")
		( "H|Help E|Emacs" :info emacs :help "emacsのマニュアル")
		( "H|Help L|Elisp" :info elisp :help "elispのマニュアル")))

(defvar tt3-menu-string-list nil) 
(defvar thinktank3-menu-before-initialize-hook nil)
(defvar thinktank3-menu-after-initialize-hook nil)

(defun tt:menu-initialize () (interactive) "   (tt:menu-initialize)
* [説明] 各所で設定されているメニュー設定値を収集し、thinktank menuを再構成する
  [注意] 
	 (thinktank3-menu-add [メニュー] )                              ; メニューを設定する
	 (add-hook 'thinktank3-menu-before-initialize-hook [初期化] )   ; 追加の初期化指定(optional)
	 (tt:menu-initialize)                                   ; 初期化
	 (define-key map (kbd ”C-/”) 'tt:menu-show-tree-menu)   ; 呼び出し"

	(run-hooks 'thinktank3-menu-before-initialize-hook)

	(setq tt3-menu-string-list (or tt3-menu-string-list tt3-menu-default-string-list))  ;; デフォルトメニューの登録

	(run-hooks 'thinktank3-menu-after-initialize-hook))


(defun thinktank3-menu-clear () (setq tt3-menu-string-list nil))
(defun thinktank3-menu-add ( items ) 
	(if (stringp (car items))
			(push items	tt3-menu-string-list)
		(setq tt3-menu-string-list (append tt3-menu-string-list items))))

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [4/6] helm自動選択
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defun tt:menu-select-one-automatically () (interactive) "
* [説明] helm menuで候補が一つに絞られると自動で第１アクションが実行される。"
	(setq tt3-menu-execute-if-one-candidate (not tt3-menu-execute-if-one-candidate))
	(message "Autoselect %s" (if tt3-menu-execute-if-one-candidate "on" "off")))

;;
;; helm候補が１つに絞られると自動実行する。
;;
(defvar tt3-menu-execute-if-one-candidate nil)
(defun tt3-menu-one-candidate ()
	(when (and (eql 1 (helm-approximate-candidate-number)) tt3-menu-execute-if-one-candidate)
		(discard-input)
		(helm-exit-minibuffer)))

(add-hook 'helm-after-update-hook 'tt3-menu-one-candidate)
;; (remove-hook 'helm-update-hook 'tt3-menu-one-candidate)

;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [5/6] コンテキスト記録
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------
(defvar tt3-menu-arg '())     ; C-uの状態を保持
(defvar tt3-menu-context '()) ; 幾つかの状態を保持

;; with-tt3-menu-contextのbody内部で使える関数。　開始時のcontext値を返す
(defun tt3-tt3-menu-context-prefix () (string-to-number (or (car (assoc-default "pre" tt3-menu-context)) "0")))
(defun tt3-tt3-menu-context-filext () (car (assoc-default "ext" tt3-menu-context)))
(defun tt3-tt3-menu-context-majmod () (car (assoc-default "maj" tt3-menu-context)))
(defun tt3-tt3-menu-context-minmod () (car (assoc-default "min" tt3-menu-context)))
(defun tt3-tt3-menu-context-seltxt () (car (assoc-default "sel" tt3-menu-context)))
(defun tt3-tt3-menu-context-buffer () (car (assoc-default "buf" tt3-menu-context)))
(defun tt3-tt3-menu-context-calendar () (car (assoc-default "cal" tt3-menu-context)))

(defmacro with-tt3-menu-context ( C-u-arg &rest body ) "
	(defun xxxx ( &optional arg ) (interactive \"P\") (with-tt3-menu-context arg (処理)) )
   で xxxx 呼び出し時の C-u 状態が記録される。 (処理)内で上記context関数が使える。
"
	`(let ((arg1 (condition-case nil (car ,C-u-arg) (error 0))))
		 ;(msgbox "copn:%s" ,C-u-arg)
		 (progn (setq tt3-menu-arg arg1)
						(setq tt3-menu-context `(("pre" ,(number-to-string (or arg1 0)))
																		 ("ext" ,(file-name-extension (buffer-name)))
																		 ("maj" ,(symbol-to-string major-mode))
																		 ("cal" ,(condition-case nil (cfw:calendar-to-emacs (cfw:cursor-to-nearest-date)) (error nil)))
																		 ("sel" ,(current-select-string))
																		 ("buf" ,(buffer-name))))
						
						;(msgbox "cco:%s / %s" tt3-menu-arg tt3-menu-context)
						;; (msgbox "cco%s" (tt3-tt3-menu-context-prefix))
						,@body
						
						(setq tt3-menu-arg nil)
						(setq tt3-menu-context nil))
		))


(defun tt3-tt3-menu-context-match ( flag ) ;; menu 表示の確認のため、flagが保存contextに合致するかどうかチェックする
	;; [2014-09-20 Sat 18:01] なんかこのままじゃ使いにくいと思っている。

	;;
	;;  flag: pre:16,ext:howm,
	;;
	;;		pre: C-u状態           ( 0, 4, 16, 64 ,,, )
	;;	　tte: element名         ( ex: paragraph, time )
	;;	　ext: ファイル拡張子    (msgbo x"%S" (file-name-extension (buffer-name)))
	;;	　maj: emacs major mode  (msgbox "%S" major-mode)
	;;    min: emacs minor mode  (msgbox "%S" minor-mode-list)
	;;	　sel: 選択状態 ( t ) 
	;;	　org: org element ( selected ) 
	;;	　buf: バッファー名 ( *scrache* )

	(or (not flag)
			(loop for item in (split-string flag ",")
						unless (destructuring-bind ( key val ) (split-string item ":") (equal (car (assoc-default key tt3-menu-context)) val))
						return nil
						finally return t)))

;; (tt3-tt3-menu-context-match "buf:tt3-menu.el")

;; ユーザー入力の収集
(defun tt3-tt3-menu-get-input ( input message ) "
* [説明] 複数情報源からユーザー入力を収集する"
	(setq message (or message "input:"))
	;; (msgbox "%S %S %S" input (current-select-string) (thing-at-point 'word) )
	(case input 
		(:select (or (current-select-string) (throw 'input-error 1)))
		(:point  (or (thing-at-point 'word) (throw 'input-error 2)))
		(:clip   (or (clipboarded-string) (throw 'input-error 3)))
		(:key    (or (and message (read-string message)) (throw 'input-error 4)))
		(:input  (or (current-select-string) (and message (read-string message)) (throw 'input-error 8)))
		(:cursor (or (current-select-string) (thing-at-point 'word) (throw 'input-error 5)))
		(:emacs  (or (current-select-string) (thing-at-point 'word) (and message (read-string message)) (throw 'input-error 6)))
		(:any    (or (current-select-string) (thing-at-point 'word) (clipboarded-string) (and message (read-string message)) (throw 'input-error 7)))
		(t "")))












;;--------------------------------------------------------------------------------------------------------------------------------------------
;;
;; [6/6] メニュー表示関数群
;;
;;--------------------------------------------------------------------------------------------------------------------------------------------

;; string-list形式メニューをtree/listで表示する
(defun tt3-menu-show-stringlist-menu ( string-list menu-type &optional input ) "
* [説明] strlig-list menuをtree/list/popup表示する
  [引数] string-list  : keymap
         menu-type    : :tree | :list | :popup 
         arg          : C-u 状態を植えとる "
	
	(flet ((do-action ( plist ) ;; stringlistのコマンドを実行
										(catch 'input-error (let* (;; ↓コマンド取得
																							 (cmd (getf plist :command))
																							 (hlm (getf plist :helm))
																							 (inf (getf plist :info))
																							 (url (getf plist :url))
																							 (fnc (getf plist :func))
																							 ;; ↓ユーザー入力処理
																							 (input (tt3-tt3-menu-get-input (getf plist :input) (getf plist :message))))
																					;; ↓コマンドの実行
																					(cond (cmd (funcall cmd))
																								(hlm (helm :sources hlm :buffer "*submenu*"))
																								(inf (info (symbol-name inf)))
																								(url (browse-url (replace-regexp-in-string "%input" (url-hexify-string input) url)))
																								(fnc (eval fnc)))))))
		
		(case menu-type
			(:tree (let* ((res (tt3-menu-popwin-show-menu (tt3-menu-stringlist-to-tree string-list))))
							 (cond ((consp res) (do-action res)) ;; 通常実行
										 ((and (stringp res) (string-match "tohelm:\\(.*\\) [^ ]+$" res)) (tt3-menu-show-stringlist-menu string-list :list (match-string 1 res)))))) ;; helmに移行

			(:list (helm :sources '((name . "thinktank action")
															(candidates . string-list)
															(candidate-transformer .  tt3-menu-stringlist-to-helm-candidates)
															(action ("D|DoAction"  . (lambda (x) (do-action (cdr x))))
																			("S|show" . (lambda (x) (msgbox "%S" (cdr x))))
																			("I|Insert" . (lambda (x) (insert (format "%S" (cdr x))))))
															)
									 :input input))
			(:popup (flet ((tree-to-keymapmenu (tree)
																				 (if (cddr tree)
																						 `(,(intern (car tree)) menu-item ,(car tree) ,(or (getf tree :command) 'test))
																					 `(,(intern (car tree)) menu-item ,(car tree) 
																						 (keymap ,(car tree) ,@(loop for branch in (cadr tree) collect (tree-to-keymapmenu branch)))))))
								(let ((tree (tt3-menu-stringlist-to-tree string-list)))
									(popup-menu (cadddr (tree-to-keymapmenu (list "ESC to cancel" tree))))))))))




;; thinktankの基本menu formatはtt3-menu-default-string-listの形式  (string list形式)
(defun tt3-menu-stringlist-to-helm-candidates ( string-list )
	(let* ((helm-candidates string-list))

		;; context 依存制御
		(setq helm-candidates (loop for item in helm-candidates
																for context = (getf (cdr item) :context)
																if (or (not context) (tt3-tt3-menu-context-match context))
																collect item))
		;; global-keymap表示
		(setq helm-candidates (loop for ( caption . params ) in helm-candidates
																collect (cons (format "%-s %8s %-s %s"
																											(adjust-string caption 40)
																											(concat "[" (mapconcat (lambda (x) (substring x 0 1)) (split-string caption " ") "") "]")
																											(adjust-string (getf params :help) 60)
																											(cond ((find :command params) (getf params :command))
																														((find :url params) (adjust-string (getf params :url) 40))
																														((find :func params) (getf params :func))
																														((find :info params) (getf params :info))
																														((find :helm params) (getf params :helm))))
																							(cons caption params))))
		helm-candidates	))

;; string-list形式から item-list形式を経て、tree形式に変換する。
(defun tt3-menu-stringlist-to-tree ( string-list )
	(let* ((item-list string-list))

		;; context 依存制御
		(setq item-list (loop for item in item-list
													for context = (getf (cdr item) :context)
													if (or (not context) 
																 (and (stringp context) (tt3-tt3-menu-context-match context))
																 (and (symbolp context) (funcall context))
																 (and (consp context) (eval context)))
													collect item))
		
		;; string-list → item-list
		(setq item-list (loop for ( caption . plist ) in item-list
													collect `(,(split-string caption " ") :item ,@plist)))

		;; global-keymap表示
		(setq item-list (loop for ( caps . params ) in item-list
													if (find :command params)
													do (let* ((key-desc (key-description (where-is-internal (getf (cdr params) :command) nil t t))))
															 (when (< 0 (length key-desc))
																 (setq caps (append (subseq caps 0 -1) (list (format "%s [%s]" (car (last caps)) key-desc))))))
													collect `(,caps ,@params)))

		;; item-list → tree
		(list2tree item-list)))





;;
;; popwin ( https://github.com/m2ym/popwin-el/tree/v0.3 ) の設定とメニュー表示
;;
(require 'popwin)

;;
;; 引数menutree は tree形式。 固定メニューは helmに適した string-list形式。
;;
;; string-list:  tt3-menu-default-string-list
;; tree:         (tt3-menu-stringlist-to-tree tt3-menu-default-string-list)
;;

(defvar tt3-menu-popwin-buffer-name "*popwin-menu*")
(defvar tt3-menu-popwin-buffer-height 15)
(push `(,tt3-menu-popwin-buffer-name :position bottom :dedicated t :stick t :height ,tt3-menu-popwin-buffer-height) popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)

(defun tt3-menu-popwin-show-menu ( menutree ) 
	(when (helm-alive-p) (error "Error: Trying to run helm within a running helm session"))
	(unwind-protect 
			(let* ((curpos '(0)) (maxline 50) seltree candtree curhigh curwidt preceding-input input-key selitems)
				
				;; popwinの設定
				(get-buffer-create tt3-menu-popwin-buffer-name)
				(make-variable-buffer-local 'global-hl-line-mode) 
				(setq global-hl-line-mode nil)				
				(thinktank-switch-ime nil)
				(popwin:display-buffer tt3-menu-popwin-buffer-name)
				
				(catch 'exit-menu
					(while t
						;; ======== メニュー表示 ========
						(setq selitems "")
						(erase-buffer) (loop for row from 0 to maxline do (insert "\n"))            ;; 空行作成
						(flet ((sort-tree (tree) (condition-case nil (sort (copy-tree tree) (lambda (x y) (string< (car x) (car y)))) (error tree))))
							(loop for curtree = (sort-tree menutree) then (sort-tree (cadr seltree))  ;; 列単位でループ
										for currow in curpos
										for col from 0 to 20
										do (progn
												 (setq curhigh (length curtree))
												 (setq curwidt (loop for item in curtree maximize (length (car item))))
												 (set-window-text-height (get-buffer-window) (if (and (< (- tt3-menu-popwin-buffer-height 1) curhigh)	(< curhigh 50)) curhigh tt3-menu-popwin-buffer-height)) ; window高調整
												 (loop for ( caption . param ) in curtree ;; メニュー描画
															 for row from 0 to maxline
															 do (progn (goto-line (+ row 1))
																				 (goto-char (line-end-position))
																				 (when (equal currow row) (setq selitems (format "%s %s" selitems caption)))
																				 (setq caption (concat (adjust-string caption curwidt)
																															 (if (equal :item (car param)) "   " " > ")))
																				 (insert (if (equal currow row) (propertize caption 'face 'highlight) caption))))
												 (loop for row from curhigh to maxline ;; 空行描画
															 do (progn
																		(goto-line (+ row 1))
																		(goto-char (line-end-position))
																		(insert (make-string (+ 3 curwidt) ? ))))
												 (setq seltree (nth currow curtree) candtree curtree))))

						;; ======== キー入力 ========
						(goto-char (point-min))
						(let* ((pos (car (last curpos))))
							(setq input-key (or preceding-input (read-key (format "%s  [%s(%s)]:" selitems (ignore-errors (char-to-string input-key)) input-key))))
							(setq preceding-input nil)
							(case input-key
								(7  (throw 'exit-menu 1)) ;; C-g
								(27 (throw 'exit-menu 2)) ;: ESC
								
								((right 13 ?\C-f) ;; right: 選択
								 (if (equal (cadr seltree) :item)
										 (throw 'exit-menu seltree)
									 (setq curpos (append curpos '(0)))))

								((left ?\C-b) ;; left
								 (if (cdr curpos)
										 (setq curpos (subseq curpos 0 -1))
									 (throw 'exit-menu 3)))

								((up ?\C-p) ;; up
								 (setq curpos (append (subseq curpos 0 -1) (list (if (= pos 0) (- curhigh 1) (- pos 1))))))
								
								((down 32 ?\C-n) ;; down
								 (setq curpos (append (subseq curpos 0 -1) (list (if (= pos (- curhigh 1)) 0 (+ pos 1))))))

								((tab 9) ;; tab
								 (throw 'exit-menu (format "tohelm:%s" selitems)))

								(t (cond ((numberp input-key) ;; initial letter jump
													(let* ((candidates (loop for ( menu . items ) in candtree ;; イニシャル一致項目のrowを得る
																									 for num from 0
																									 if (string-match (format "^%c.*" input-key) menu)
																									 collect num)))
														(cond ((= 1 (length candidates))            ;; 選択
																	 (setq curpos (append (subseq curpos 0 -1) candidates))
																	 (if (equal :item (cadr (nth (car candidates) candtree))) 
																			 (setq preceding-input 'right)
																		 (setq curpos (append curpos '(0)))))
																	((< 1 (length candidates))            ;; 複数なら次の候補に移動
																	 (setq curpos (append (subseq curpos 0 -1) (list (or (find-if (lambda (x) (< pos x)) candidates) (car candidates)))))))))
												 (t (msgbox "%c" input-key))
												 )
									 ))))))
		
		;; popwinの後始末
		(ignore-errors
			(popwin:close-popup-window)
			(kill-buffer tt3-menu-popwin-buffer-name))))


(provide 'tt3-menu)
