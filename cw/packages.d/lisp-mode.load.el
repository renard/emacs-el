(defun cw:emacs-lisp-mode-setup ()
  "Setup for `emacs-lisp-mode'."
  (when (or (not (boundp 'cw:yasnippet:in-expansionp))
	    (and (boundp 'cw:yasnippet:in-expansionp)
		 (not cw:yasnippet:in-expansionp)))
    (unless (boundp 'cw:org:publishing-project)
      (hs-minor-mode)
      (flyspell-prog-mode)))
  (rainbow-delimiters-mode 1))

(add-hook 'emacs-lisp-mode-hook 'cw:emacs-lisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-mode-hook 'cw:emacs-lisp-mode-setup)
