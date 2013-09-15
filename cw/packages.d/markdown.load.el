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



