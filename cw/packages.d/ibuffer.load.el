(setq
      ibuffer-saved-filter-groups
      '(("default"
	 ("dired" (mode . dired-mode))
	 ("erc" (mode . erc-mode))
	 ("emacs" (or
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Messages\\*$")
		   (mode . completion-list-mode)
		   (name . "\\*ielm\\*")))
	 ("lisp" (mode . emacs-lisp-mode))
	 ("gnus" (or
		  (mode . message-mode)
		  (mode . bbdb-mode)
		  (mode . mail-mode)
		  (mode . gnus-group-mode)
		  (mode . gnus-summary-mode)
		  (mode . gnus-article-mode)
		  (name . "^\\.bbdb$")
		  (name . "^\\.newsrc-dribble")
		  (name . "^\\*nnimap ")
		  (name . "^\\*imap log\\*")))
	 ("org" (mode . org-mode))
	 ("emms" (or
		  (mode . emms)))
	 ("adoc" (or
		  (mode . adoc-mode)))

	 ("tramp" (or
		   (name . "^\\*tramp")))
	 ("term" (or
		  (mode . term-mode)
		  (mode . shell-mode)))
	 ("magit" (or
		   (name . "^\\*magit"))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(defadvice ibuffer (around cw:ibuffer activate)
  "Open ibuffer with cursour pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
