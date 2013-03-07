;; do not use ~/.emacs.d/snippets as a directory
(setq yas-snippet-dirs
      (list (concat user-emacs-directory "templates")
	    (when yas--load-file-name
	      (concat (file-name-directory yas--load-file-name) "snippets"))))



