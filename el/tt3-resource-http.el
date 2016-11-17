;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-resource-http.el  thinktank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; webrickへ直接アクセス。　http-requestを出す
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'url)
(require 'json)

(defun* tt3-resource-alist2string ( alist )
	;; lookup=....&optional=....
	(join-string (loop for ( key . val ) in alist
										 collect (format "%s=%s"
																		 (url-hexify-string (cond ((symbolp key) (symbol-to-string key))
																															(t key)))
																		 (url-hexify-string (cond ((vectorp val) (json-encode val))
																															((listp val)   (json-encode val))
																															((numberp val) (number-to-string val))
																															((symbolp val) (symbol-name val))
																															(t val))))
										 ) "&" ))


'((tt3-resource-request-http))

(defun* tt3-resource-request-http ( &key (action :show) (host "0.0.0.0") (port "20091") (resource "memos") (ext "howm") (id "0000-00-00-000000")  query (body "") ) "
* [説明] thinktank serverにアクセスし、res.headerをalistで返す。　res.bodyはbufferに保存されている。
  [引数] action   :  :show, :create, :update, :destroy, :index
         id       :  \"xxxx-xx-xx-xxxxxx\"
         query    :  alist
         body     :  serverへの送信テキスト
  [返値] res.headerのalist(下記)
         bufferに response body が返る。

	;; [返値] (assoc-default \"Buffer\" (tt3-resource-request :id \"0000-00-00-000000\"))で値を取得

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
				
				(let* ((response-buf "*http-res*")
							 url-request-data 
							 url-request-extra-headers)
					;;
					;; url, url-request-method, url-request-data, url-request-extra-headers を設定して、(url-retrieve-synchronously url) を実行するとbufferにresponseが返る。 (url.el)
					;;
					(destructuring-bind ( url-request-method . url )
							(case action
								(:new     `("GET"    . ,(concat "http://" host ":" port "/thinktank/" resource             "." ext "?" (tt3-resource-alist2string query))))
								(:edit    `("GET"    . ,(concat "http://" host ":" port "/thinktank/" resource "/" id "/edit." ext "?" (tt3-resource-alist2string query))))
								(:show    `("GET"    . ,(concat "http://" host ":" port "/thinktank/" resource "/" id      "." ext "?" (tt3-resource-alist2string query))))
								(:update  `("PUT"    . ,(concat "http://" host ":" port "/thinktank/" resource "/" id      "." ext "?" (tt3-resource-alist2string query))))
								(:destroy `("DELETE" . ,(concat "http://" host ":" port "/thinktank/" resource "/" id      "." ext "?" (tt3-resource-alist2string query))))
								(:index   `("GET"    . ,(concat "http://" host ":" port "/thinktank/" resource             "." ext "?" (tt3-resource-alist2string query)))))

						(setq url-request-data           (url-hexify-string (encode-coding-string (or body "") 'utf-8-unix))) ; 最後に不必要な改行2文字が入る可能性あり
						(setq url-request-extra-headers `(("Content-Type"   . "text/howm")
																							("Content-Length" . ,(format "%d" (length url-request-data)))))

						(save-excursion (condition-case nil
																(let (heading-lines)
																	(force-generate-new-buffer response-buf)
																	(insert (decode-coding-string
																					 (with-current-buffer (url-retrieve-synchronously url) (buffer-string)) ;; ← http通信でテキストを得る
																					 'utf-8-unix))
																	
																	;; 最初の改行x2でheader/bodyに分割、headerをalist化して返す。　bodyはalistのbufferに残る。
																	(goto-char (point-min)) (re-search-forward "\n\n")
																	(setq heading-lines (split-string (buffer-substring (point) (point-min)) "\n"))
																	(delete-region (point) (point-min))
																	;; alist化
																	(cons (cons "Buffer" (current-buffer))                           ;; ("Buffer" . "buffer-name")
																				(loop for lin in heading-lines
																							if (string-match "\\(: \\|HTTP\\)" lin)              
																							collect (let ((tmp (split-string lin "\\(: \\)")))
																												(case (length tmp)
																													(1 (cons "Status" (car tmp)))            ;; ("Status" . "HTTP/1.1 200 OK" )
																													(2 (cons (car tmp) (cadr tmp))))))))     ;; ("key" . "value" )
															(error "error" '(("Status" . "error"))))))))




(provide 'tt3-resource-http)

