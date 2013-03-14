(defadvice mail-envelope-from
  (around cw:mail-envelope-from activate)
  "Get envelope sender from Sender, Return-path, From fields
or `mail-envelope-from'."
  (setq ad-return-value
	(or
	 (nth 1 (mail-extract-address-components
		 (or
		  (message-fetch-field "Sender")
		  (message-fetch-field "Return-path")
		  (message-fetch-field "From"))))
	 mail-envelope-from)))

(setq
 mail-specify-envelope-from t
 mail-envelope-from 'header
 send-mail-function 'smtpmail-send-it
 smtpmail-smtp-server "127.0.0.1"
 default-sendmail-coding-system 'utf-8-unix
 message-from-style 'angles)
