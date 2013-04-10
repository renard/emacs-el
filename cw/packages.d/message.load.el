(setq message-signature-insert-empty-line t)

(add-hook 'message-mode-hook 'cw:gnus:configure-group)
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'flyspell-buffer)

(define-key message-mode-map (kbd "<tab>") 'bbdb-complete-mail)
