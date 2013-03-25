;; do not use ~/.emacs.d/snippets as a directory
(setq yas-snippet-dirs
      (list (concat user-emacs-directory "templates")
	    (when yas--load-file-name
	      (concat (file-name-directory yas--load-file-name) "snippets"))))

(defun cw:yas-exit-all-snippets()
  "Execute `yas-exit-all-snippets' within a `save-excursion' block."
  (interactive)
  (save-excursion (yas-exit-all-snippets)))

(define-key yas-keymap (kbd "C-g") 'cw:yas-exit-all-snippets)
