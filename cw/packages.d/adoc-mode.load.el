(defun cw:adoc-mode-setup ()
  "Setup `adoc-mode'."
  (setq time-stamp-start "^:date: ")
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^:lang:*\s-*\\(.+\\)" nil t 1)
	(let ((lang (match-string 1)))
	  (flyspell-mode)
	  (ispell-change-dictionary
	   (cond
	    ((string= "fr" lang) "francais")
	    (t "american"))))))))
(define-key adoc-mode-map (kbd "C-c m") 'cw:adoc-mode:compile)
(set-face-attribute 'adoc-hide-delimiter nil :height 1.0)
