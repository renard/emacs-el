;;;###autoload
(defun cw:yasnippet:insert-maybe ()
  ""
  (interactive)
  (when
      (and
       (not noninteractive)
       (buffer-file-name)
       (not (file-exists-p (buffer-file-name)))
       (= (point-max) 1))
    (let ((mode (symbol-name major-mode)))
      (loop for d in yas-snippet-dirs
	    when (file-exists-p
		  (format "%s/%s/new-file-tpl.yasnippet" d mode))
	    do (progn
		 ;;(message "Found: %s/%s/new-file-tpl.yasnippet" d mode)
		 (insert "new-file-tpl")
		 ;; reload templates before expansion
		 (yas-reload-all)
		 (yas-expand))
	  and return t))))

(setq yas-snippet-dirs
      (list (concat user-emacs-directory "templates")
	    (when yas--load-file-name
	      (concat (file-name-directory yas--load-file-name) "snippets"))))



