(setq message-signature-insert-empty-line t)
(add-hook 'message-mode-hook 'cw:gnus:configure-group)
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'flyspell-buffer)
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
;;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
     
;; (setq bbdb/news-auto-create-p t)
;; (setq bbdb/gnus-summary-user-format-letter nil
;; 	   bbdb/gnus-summary-in-bbdb-format-letter nil)

(define-key message-mode-map (kbd "<tab>") 'bbdb-complete-mail)
