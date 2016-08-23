;;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; tt3-oauth2.el  thinktank

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [2014-07-16 水 18:29]
;; thinktank3-resource-get-system-property で読み取ろうとすると、webrick起動が追いつかないためエラーになる。
;; 起動を待つか、thinktank3-property を使う。

;;============================================================================================================================================
;; Googlecal OAuth2 : https://developers.google.com/google-apps/calendar/v3/reference/
;;============================================================================================================================================
(defvar tt3-oauth2-googlecal-client-id     (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "client-id"))     ;; 事前にAPIユーザーとしてのidを取得しておく必要がある。
(defvar tt3-oauth2-googlecal-client-secret (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "client-secret"))
(defvar tt3-oauth2-googlecal-redirect-uri  (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "redirect-uri")) 
(defvar tt3-oauth2-googlecal-access-token  (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "access-token"))
(defvar tt3-oauth2-googlecal-expiration    (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "expiration"))
(defvar tt3-oauth2-googlecal-refresh-token (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "refresh-token"))
(defvar tt3-oauth2-googlecal-token-type    (thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "token-type"))

(defun* tt3-oauth2-googlecal-get-access-token ()
	(let* (auth-code url-request-method url-request-data url-request-extra-headers)

		(when (cond (;; 新規登録 -----------------------------------------------------------------------------------------------------------------
								 (or (= 0 (length tt3-oauth2-googlecal-access-token)) (= 0 (length tt3-oauth2-googlecal-refresh-token)))
								 ;; ブラウザで認証サイト接続＆認証コード取得
								 (browse-url  (concat "https://accounts.google.com/o/oauth2/auth"
																			"?client_id=" tt3-oauth2-googlecal-client-id
																			"&response_type=code"
																			"&redirect_uri=" tt3-oauth2-googlecal-redirect-uri
																			"&scope=https://www.googleapis.com/auth/calendar" ))
								 (setq auth-code (tt3-oauth2-get-redirect-url tt3-oauth2-googlecal-redirect-uri)) ; httpdでリダイレクトとして受ける。　承認を押すだけ。
								 
								 ;; 認証コードを用いてアクセストークン取得
								 (setq url-request-data (concat "code=" auth-code
																								"&grant_type=authorization_code"
																								"&client_id="     tt3-oauth2-googlecal-client-id
																								"&client_secret=" tt3-oauth2-googlecal-client-secret
																								"&redirect_uri="  tt3-oauth2-googlecal-redirect-uri "&" )) ;; 最後の&は必須
								 t)

								(;; アクセストークン取得済みで期限切れ ---------------------------------------------------------------------------------------
								 (string< tt3-oauth2-googlecal-expiration (format-time-string "%Y-%m-%d-%H%M%S"))
								 
								 ;; リフレッシュトークンを用いてアクセストークン再取得
								 (setq url-request-data (concat "grant_type=refresh_token"
																								"&refresh_token=" tt3-oauth2-googlecal-refresh-token
																								"&ver=3&"))
								 t)
								
								(t nil)) ;; トークン再取得の必要なし -----------------------------------------------------------------------------------------

			(let* (http-res alist)
				(setq gnutls-min-prime-bits     1024)
				(setq url-request-method        "POST")
				(setq url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
				(setq http-res                  (replace-regexp-in-string "\n" "" (url-retrieve-string "https://accounts.google.com/o/oauth2/token"))) ; httpアクセス
				(setq alist (when (string-match "\\(\{.*\}\\)" http-res) (json-read-from-string (match-string 1 http-res))))

				(setq tt3-oauth2-googlecal-access-token  (assoc-default 'access_token alist))
				(setq tt3-oauth2-googlecal-refresh-token (assoc-default 'refresh_token alist))
				(setq tt3-oauth2-googlecal-token-type    (assoc-default 'token_type alist))
				(setq tt3-oauth2-googlecal-expiration    (format-time-string "%Y-%m-%d-%H%M%S" (time-add (current-time) (seconds-to-time (- (assoc-default 'expires_in alist) 600)))))
				
				(thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "access-token"  tt3-oauth2-googlecal-access-token)
				(thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "refresh-token" tt3-oauth2-googlecal-refresh-token)
				(thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "expiration"    tt3-oauth2-googlecal-expiration)
				(thinktank3-property "Thinktank.Synchronization.GoogleCalendar" "token-type"    tt3-oauth2-googlecal-token-type)
				))
		
		tt3-oauth2-googlecal-access-token))


;;============================================================================================================================================
;; Toodledo OAuth2 : https://api.toodledo.com/3/account/doc_register.php?edit=Thinktank3
;;============================================================================================================================================
(defvar tt3-oauth2-toodledo-client-id     (thinktank3-property "Thinktank.Synchronization.Toodledo" "client-id"))     ;; 事前のAPIユーザー登録無し、代りにredirect時にBasic認証が必要。
(defvar tt3-oauth2-toodledo-client-secret (thinktank3-property "Thinktank.Synchronization.Toodledo" "client-secret"))
(defvar tt3-oauth2-toodledo-redirect-uri  (thinktank3-property "Thinktank.Synchronization.Toodledo" "redirect-uri"))  ;; toodledoはclient側でredirect urlを指定できないので上記urlに直接設定している。
(defvar tt3-oauth2-toodledo-access-token  (thinktank3-property "Thinktank.Synchronization.Toodledo" "access-token"))
(defvar tt3-oauth2-toodledo-refresh-token (thinktank3-property "Thinktank.Synchronization.Toodledo" "refresh-token"))
(defvar tt3-oauth2-toodledo-token-type    (thinktank3-property "Thinktank.Synchronization.Toodledo" "token-type"))
(defvar tt3-oauth2-toodledo-expirtion     (thinktank3-property "Thinktank.Synchronization.Toodledo" "expiration"))

(defun* tt3-oauth2-toodledo-get-access-token () 
	(let* (auth-code url-request-method url-request-data url-request-extra-headers)

		(when (cond (;; 新規登録 -----------------------------------------------------------------------------------------------------------------
								 (or (= 0 (length tt3-oauth2-toodledo-access-token)) (= 0 (length tt3-oauth2-toodledo-refresh-token)))
								 ;; ブラウザで認証サイト接続＆認証コード取得
								 (browse-url  (concat "https://api.toodledo.com/3/account/authorize.php"
																			"?response_type=code"
																			"&client_id=" tt3-oauth2-toodledo-client-id
																			"&state=sddfsfhjhkjs"
																			"&scope=basic&" ))
								 (setq auth-code (tt3-oauth2-get-redirect-url tt3-oauth2-toodledo-redirect-uri)) ; httpdでリダイレクトを受ける。　Basic認証(id/password)が必要。
								 
								 ;; 認証コードを用いてアクセストークン取得
								 (setq url-request-data (concat "grant_type=authorization_code"
																								"&code=" auth-code
																								"&ver=3&" ))
								 t)
					
								(;; アクセストークン取得済みで期限切れ ---------------------------------------------------------------------------------------
								 (string< tt3-oauth2-toodledo-expirtion (format-time-string "%Y-%m-%d-%H%M%S"))
								 
								 ;; リフレッシュトークンを用いてアクセストークン再取得
								 (setq url-request-data (concat "grant_type=refresh_token"
																								"&refresh_token=" tt3-oauth2-toodledo-refresh-token
																								"&ver=3&"))
								 t)

								(t nil)) ;; トークン再取得の必要なし -----------------------------------------------------------------------------------------

			(let* (http-res alist)
				(setq gnutls-min-prime-bits     1024)
				(setq url-request-method        "POST")
				(setq url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
																					("Authorization" . ,(concat "Basic " (base64-encode-string (concat tt3-oauth2-toodledo-client-id ":" tt3-oauth2-toodledo-client-secret))))))
				(setq http-res                  (replace-regexp-in-string "\n" "" (url-retrieve-string "https://api.toodledo.com/3/account/token.php"))) ; httpアクセス
				(setq alist (when (string-match "\\(\{.*\}\\)" http-res) (json-read-from-string (match-string 1 http-res))))

				; (setq tt3-oauth2-toodledo-token-alist (when (string-match "\\(\{.*\}\\)" http-res) (json-read-from-string (match-string 1 http-res))))
				(setq tt3-oauth2-toodledo-refresh-token (assoc-default 'refresh_token alist))
				(setq tt3-oauth2-toodledo-access-token  (assoc-default 'access_token alist))
				(setq tt3-oauth2-toodledo-token-type    (assoc-default 'token_type alist))
				(setq tt3-oauth2-toodledo-expiration    (format-time-string "%Y-%m-%d-%H%M%S" (time-add (current-time) (seconds-to-time (- (assoc-default 'expires_in alist) 600)))))

				(thinktank3-property "Thinktank.Synchronization.Toodledo" "access-token"  tt3-oauth2-toodledo-access-token)
				(thinktank3-property "Thinktank.Synchronization.Toodledo" "refresh-token" tt3-oauth2-toodledo-refresh-token)
				(thinktank3-property "Thinktank.Synchronization.Toodledo" "expiration"    tt3-oauth2-toodledo-expiration)
				(thinktank3-property "Thinktank.Synchronization.Toodledo" "token-type"    tt3-oauth2-toodledo-token-type)
				))

		tt3-oauth2-toodledo-access-token))


;;============================================================================================================================================
;; Pocket OAuth2 : http://getpocket.com/developer/docs/authentication
;;============================================================================================================================================
;; [[http://www.koikikukan.com/archives/2014/04/01-015555.php][PocketでOAuth2認証を行う方法: 小粋空間]]
;; [[http://apitip.com/pocket/4][アイテムリストを出力する [Retrieve] - pocket APIの使い方まとめ]]

(defvar tt3-oauth2-pocket-application-name (thinktank3-property "Thinktank.Synchronization.Pocket" "application-name"))
(defvar tt3-oauth2-pocket-consumer-key     (thinktank3-property "Thinktank.Synchronization.Pocket" "consumer-key"))     ;; client-secretのようなもの
(defvar tt3-oauth2-pocket-redirect-uri     (thinktank3-property "Thinktank.Synchronization.Pocket" "redirect-uri"))
(defvar tt3-oauth2-pocket-access-token     (thinktank3-property "Thinktank.Synchronization.Pocket" "access-token"))
(defvar tt3-oauth2-pocket-username         (thinktank3-property "Thinktank.Synchronization.Pocket" "username"))

(defun* tt3-oauth2-pocket-get-access-token ()
	(let* (auth-code url-request-method url-request-extra-headers url-request-data http-res alist) ;; auth-code → code/request_token

		(when (= 0 (length tt3-oauth2-pocket-access-token))
			;; リクエストトークンを取得
			(setq gnutls-min-prime-bits 1024)
			(setq url-request-method        "POST")
			(setq url-request-extra-headers '(("Content-Type" . "application/json; charset=UTF-8") ("X-Accept" . "application/json")))
			(setq url-request-data          (json-encode `(:consumer_key ,tt3-oauth2-pocket-consumer-key 
																																	 :redirect_uri tt3-oauth2-pocket-redirect-uri
																																	 :state        "asdlkajsdllllkjll")))
			(setq http-res                  (replace-regexp-in-string "\n" "" (url-retrieve-string "https://getpocket.com/v3/oauth/request")))
			(setq alist (when (string-match "\\(\{.*\}\\)" http-res) (json-read-from-string (match-string 1 http-res))))
			(setq auth-code (assoc-default 'code alist))
			
			;; ユーザーによるログイン＆認証
			(browse-url (concat "https://getpocket.com/auth/authorize"
													"?redirect_uri=" tt3-oauth2-pocket-redirect-uri
													"&request_token=" auth-code
													"&" ))
			(tt3-oauth2-get-redirect-url tt3-oauth2-pocket-redirect-uri) ; httpdでリダイレクトを受ける。　Google認証またはPassword認証(id/password)が必要。
			
			;; アクセストークンを取得
			(setq gnutls-min-prime-bits     1024)
			(setq url-request-method        "POST")
			(setq url-request-extra-headers '(("Content-Type" . "application/json; charset=UTF-8") ("X-Accept" . "application/json")))
			(setq url-request-data          (json-encode `(:consumer_key ,tt3-oauth2-pocket-consumer-key :code ,auth-code)))
			(setq http-res                  (replace-regexp-in-string "\n" "" (url-retrieve-string "https://getpocket.com/v3/oauth/authorize")))
			(setq alist                     (when (string-match "\\(\{.*\}\\)" http-res) (json-read-from-string (match-string 1 http-res))))
			
			(setq tt3-oauth2-pocket-access-token (assoc-default 'access_token alist))
			(setq tt3-oauth2-pocket-username (assoc-default 'username alist))
			
			(thinktank3-property "Thinktank.Synchronization.Pocket" "access-token" tt3-oauth2-pocket-access-token)
			(thinktank3-property "Thinktank.Synchronization.Pocket" "username" tt3-oauth2-pocket-username)
			)
		
		tt3-oauth2-pocket-access-token))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; localhost:8888へのredirectを受けるためhttp serverを起動し待機。
;;
(defvar tt3-oauth2-auth-code nil)
(defun tt3-oauth2-get-redirect-url ( local-uri ) ;; http://localhost:8888 とか
	(let (proclist)
		(setq tt3-oauth2-auth-code nil)
		(setq proclist (process-list))
		(make-network-process :name    "httpd/localhost" 
													:server  t 
													:host    'local 
													:service (string-to-number (and (string-match ":\\([0-9]+\\)" local-uri) (match-string 1 local-uri)))
													:filter  (lambda (process str)
																		 (let* ((param (find-if (lambda (x) (string-match "^GET" x)) (split-string str "\n")))
																						(code  (if (string-match "code=\\([^& ]*\\)" param) (match-string 1 param) "nocode")))
																			 (process-send-string process (concat "HTTP/1.0 200 OK\n"
																																						"Content-Type: text/html\n\n\n"
																																						"<html><head><title></title></head>"
																																						"<body onLoad=\"setTimeout('window.close()',1000);\">" ;; firefoxは無効
																																						"Thank you for your authorization of Thinktank3. </body></html>\n"))
																			 (setq tt3-oauth2-auth-code code)
																			 )))
		(while (null tt3-oauth2-auth-code) (sleep-for 0 100) (message "browserで認証してください")) ;; serverプロセス終了するとループから抜けられる。
		(mapc '(lambda (p) (unless (find p proclist) (delete-process p))) (process-list)) ;; (mapc 'delete-process (process-list))
		tt3-oauth2-auth-code))

		

(provide 'tt3-oauth2)
