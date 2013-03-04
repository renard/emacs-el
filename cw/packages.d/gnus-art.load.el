(setq
 gnus-visible-headers (concat gnus-visible-headers
			      "\\|^User-Agent:\\|^X-Mailer:\\|^X-Spam.score:")
 gnus-article-update-date-headers nil)
(define-key gnus-article-mode-map (kbd "<C-return>") 'gnus-article-browse-html-article)
