(defun cw:puppet-mode:align ()
  "Align paragraph on \"=\" character as defined in puppet
       best practices style guide."
  (interactive)
  (save-excursion
    (let ((beg (progn (backward-paragraph 1) (point)))
	  (end (progn (forward-paragraph 1) (point))))
      (indent-region beg end)
      (align-regexp beg end "\\(\\s-*\\) \\(=\\|#\\)" -1 0 nil))))

(defun cw:puppet-mode:newline-and-indent ()
  "Align paragraph using `cw:puppet-mode:align' before newline."
  (interactive)
  (cw:puppet-mode:align)
  (newline-and-indent))

(defun cw:puppet-mode:indent-for-tab-command (&optional arg)
  "Align paragraph using `cw:puppet-mode:align' before indenting."
  (interactive)
  (cw:puppet-mode:align)
  (indent-for-tab-command arg))

(defun cw:puppet-add-before-save-hook ()
  "Run `delete-trailing-whitespace' before saving a puppet file."
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))


(add-hook 'puppet-mode-hook 'cw:puppet-add-before-save-hook)
(define-key puppet-mode-map (kbd "RET") 'cw:puppet-mode:newline-and-indent)
(define-key puppet-mode-map (kbd "TAB") 'cw:puppet-mode:indent-for-tab-command)
