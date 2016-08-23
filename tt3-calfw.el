;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt-calfw.el  thinktank

;; calfw
(require 'calfw)
(require 'calfw-ical)
(require 'japanese-holidays)

(setq calendar-holidays (append japanese-holidays local-holidays other-holidays))
(setq mark-holidays-in-calendar t)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
(setq calendar-week-start-day 1) ;; 日曜日は0, 月曜日は1

;;=============================================================================================================================================
;; Public
;;==============================================================================================================================================
(defvar tt3-calfw-ggl-job-ics  "https://www.google.com/calendar/ical/gogowooky%40gmail.com/private-103647c50c368a392c3c6f1bd2c28ff1/basic.ics")
(defvar tt3-calfw-ggl-priv-ics "https://www.google.com/calendar/ical/arnpo4mc349h4q544ctgmbml1s%40group.calendar.google.com/private-775e1f46ff0041c8bfebd3373bd95208/basic.ics")

'((cfw:ical-create-source "仕事ggl" tt3-calfw-ggl-job-ics "firebrick"))

(defun tt:calfw-show ()	(interactive)
	(thinktank3-calfw-open-calendar :callist (split-string (thinktank3-property "Extension.Queries.Calfw" "cal-set") "[, ]" nil)))

(defun tt:calfw-show-group () (interactive)
	(let* ((cal-group    (tt3-mapcar-property-subnode "Extension.Queries.Calfw" (cons title (org-entry-get nil "cal-group"))))
				 (group        (delete-duplicates (loop for (name . grpstr) in cal-group
																								if grpstr append (split-string grpstr "[, ]" t)) :test 'equal))
				 (group-cal    (mapcar (lambda (grp) (cons (capitalize grp) (loop for ( name . grpstr ) in cal-group
																																					if (and grpstr (string-match grp grpstr)) collect name)))
															 group)))
    (helm :sources `((name . "CalendarGroup")
                     (candidates . ,group-cal)
                     (action ( "S|Show" . (lambda (x) (thinktank3-calfw-open-calendar :callist x))))))))

(defun tt:calfw-update () (interactive)
	(let* ((cal-set   (split-string (thinktank3-property "Extension.Queries.Calfw" "cal-set") "[, ]" nil))
				 (calendars (delq nil (tt3-mapcar-property-subnode "Extension.Queries.Calfw" (if (org-entry-get nil "cal-group") (cons (format "%-30s | %s" (org-entry-get nil "caption") title ) title))))))
    (helm :sources `((name . "CalendarForUpdate")
                     (candidates . ,calendars)
                     (action ( "S|Show" . (lambda (x)
																						(loop for calendar in (helm-marked-candidates) 
																									do (tt:resource-index :name calendar :cache :no :output :no))
																						(cfw:refresh-calendar-buffer nil)
																						'(thinktank3-calfw-open-calendar :callist cal-set))))))))

(defun tt:calfw-show-one () (interactive)
	(let* ((calendars (delq nil (tt3-mapcar-property-subnode "Extension.Queries.Calfw" (if (org-entry-get nil "cal-group") (cons (format "%-30s | %s" (org-entry-get nil "caption") title ) title))))))
    (helm :sources `((name . "CalendarForShowOne")
                     (candidates . ,calendars)
                     (action ( "S|Show" . (lambda (x) (thinktank3-calfw-open-calendar :callist (list x)))))))))

(defun tt:calfw-select () (interactive)
	(let* ((cal-set   (split-string (thinktank3-property "Extension.Queries.Calfw" "cal-set") "[, ]" nil))
				 (preselect (format "\\(%s\\)" (mapconcat 'identity cal-set "\\|")))
				 (calendars (delq nil (tt3-mapcar-property-subnode "Extension.Queries.Calfw" (if (org-entry-get nil "cal-group") (cons (format "%20s | %s" (org-entry-get nil "caption") title ) title)))))
         func)

    (fset 'func `(lambda () (with-helm-window
                              (goto-char (point-min))
                              (while (and (< (point) (point-max)) (< (point) (progn (helm-next-line) (point))))
                                (if (string-match ,preselect (current-line-string))
                                    (helm-aif (helm-this-visible-mark) nil (helm-make-visible-mark))
                                  (helm-aif (helm-this-visible-mark) (helm-delete-visible-mark it) nil)))
                              (loop for i from 0 to 30 do (helm-previous-line)))
                   (remove-hook 'helm-after-update-hook 'func)))
		
    (add-hook 'helm-after-update-hook 'func)
    
    (helm :sources `((name . "SelectCalendar")
                     (candidates . ,calendars)
                     (candidate-number-limit . 1000 )
                     (action ( "S|Select" . (lambda (x)
																							(thinktank3-property "Extension.Queries.Calfw" "cal-set" (mapconcat 'identity (helm-marked-candidates) ","))
																							(thinktank3-calfw-open-calendar :callist (helm-marked-candidates))
																							))))
					:buffer "*temp*")))

(defun thinktank3-calfw-open-calendar ( &rest plist ) (interactive)
	(let* ((callist (getf plist :callist))
				 (calbuf (cfw:open-calendar-buffer
									:contents-sources (loop for cal in callist collect (tt3-calfw-memo2 cal))
									:date (getf plist :date)
									:view (or (getf plist :view) 'month)
									:custom-map (cfw:define-keymap '(("q"   . bury-buffer)
																									 ("SPC" . tt3-calfw-day-detail)
																									 ([mouse-1] . (lambda () (interactive) (tt3-resource-show-memo :memoid (get-text-property (point) 'memoid) :jump (get-text-property (point) 'jump))))
																									 ([return]  . (lambda () (interactive) (tt3-resource-show-memo :memoid (get-text-property (point) 'memoid) :jump (get-text-property (point) 'jump))))
																									 ))
									:sorter '(lambda (x y) (string-lessp x y))
									)))
		(set-buffer calbuf)
		(make-variable-buffer-local 'global-hl-line-mode)	(setq global-hl-line-mode nil)				
		(make-variable-buffer-local 'transient-mark-mode) (setq transient-mark-mode nil)))

(defun tt3-calfw-day-detail () (interactive) ;; calendar日関連のobjectをすべて表示する
	(let ((date (format-time-string "%Y-%m-%d" (cfw:calendar-to-emacs (cfw:cursor-to-nearest-date)))))
		(tt:resource-index :name "Extension.Queries.Memo.DayItem" :input date)))

(defadvice org-follow-timestamp-link (around tt3-calfw-timestamp-link activate) ;; timestamp clickでcalendarを開く
	(when (org-at-timestamp-p t)
		(thinktank3-calfw-open-calendar 
		 ;; :callist (split-string (thinktank3-property "Extension.Queries.Calfw" "cal-set") "[, ]" nil)

		 :callist '("Extension.Queries.Calfw.Event.Coming" "Extension.Queries.Calfw.Event.Done" "Extension.Queries.Calfw.Event.Completed" "Extension.Queries.Calfw.Event.Passed")
		 :date (calendar-gregorian-from-absolute (time-to-days (org-read-date t t (match-string 1)))))))




;; (insert (format "%S" (tt:resource-index :name "Calfw.Event.ComingPeriod" :output :lisp)))
;;==============================================================================================================================================
;; Private
;;==============================================================================================================================================

(defun* tt3-calfw-memo2 ( property-name )
	(let* ((schedule '())
				 (color   (or (thinktank3-property property-name "cal-color") "#000000"))
				 (caption (or (thinktank3-property property-name "caption") property-name))
				 (face    (thinktank3-property property-name "face"))
				 (append  (thinktank3-property property-name "append"))
				 
				 (create-schedule `(lambda (begin end)
														 (let (schedule periods)
															 
															 ;; 始点のみのイベント
															 (loop for (eventtime . plist) in (tt:resource-index :name ,property-name :output :lisp)
																		 for memoid = (getf plist :memoid)
																		 for jump = (getf plist :jump)
																		 for caption = (propertize (getf plist :caption) 'memoid memoid 'jump jump 'mouse-face 'highlight)
																		 if (cfw:date-between begin end eventtime)
																		 do (if (assoc eventtime schedule)
																						(push caption (cdr (assoc eventtime schedule)))
																					(push `(,eventtime . (,caption)) schedule)))

															 ;; 期間のあるイベント
															 (when ,append
																	(loop for (eventtime . plist) in (tt:resource-index :name ,(concat property-name "." append) :output :lisp)
																				for memoid = (getf plist :memoid)
																				for jump = (getf plist :jump)
																				for caption = (propertize (getf plist :caption) 'memoid memoid 'jump jump 'mouse-face 'highlight)
																				unless (or (and (cfw:date-less-equal-p (car eventtime) begin) (cfw:date-less-equal-p (cadr eventtime) begin))
																									 (and (cfw:date-less-equal-p end (car eventtime)) (cfw:date-less-equal-p end (cadr eventtime))))
																				do (push `(,@eventtime ,caption) periods))
																	(setq schedule (append schedule `((periods ,@periods)))))
															 
															 schedule))))

		(setq face (and face (car (read-from-string face))))
		(make-cfw:source :name caption :color color :data create-schedule :opt-face face :opt-period-face face)))



(provide 'tt3-calfw)
