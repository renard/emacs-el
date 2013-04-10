(bbdb-initialize 'gnus 'message)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
     
(setq bbdb/gnus-summary-user-format-letter nil
      bbdb/gnus-summary-in-bbdb-format-letter nil
      bbdb/news-auto-create-p nil
      bbdb-complete-mail-allow-cycling t)
