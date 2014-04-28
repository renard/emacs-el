(define-key markdown-mode-map (kbd "M-[") 'escreen-goto-prev-screen)
(define-key markdown-mode-map (kbd "M-]") 'escreen-goto-next-screen)

(defun cw:markdown-buffer-to-html()
  "Convert current buffer to HTML"
  (interactive)
  (let ((buff-src (current-buffer))
	(file (file-name-sans-extension (buffer-file-name)))
	(buff-out (get-buffer-create "*PANDOC*")))
    (with-current-buffer buff-out
      (erase-buffer))
    (call-process-region (point-min) (point-max)
			 "pandoc" nil buff-out nil "-s")
    (with-temp-file (concat file ".html")
      (insert (with-current-buffer buff-out (buffer-string))))))


(defun cw:markdown-mode-setup ()
  "Setup `markdown-mode'."
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

(add-hook 'markdown-mode-hook 'cw:markdown-mode-setup)
