(defun cw:org:org-mode-setup ()
  "Setup buffer for `org-mode' files."
  (unless (and
	   (not noninteractive)
	   (boundp 'cw:org:publishing-project)
	   cw:org:publishing-project)
    (setq time-stamp-start "^#\\+DATE: ")
    ;; flyspell mode to spell check everywhere
    (flyspell-mode 1)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
	(when (search-forward-regexp
	       "^#\\+LANG:[ \t]+\\(.*\\)"
	       nil t)
	  (message (format "Loading dict %s" (match-string 1)))
	  (ignore-errors
	    (ispell-change-dictionary (match-string 1)))))

      (goto-char (point-min))
      (save-match-data
	(when (search-forward-regexp
	       "^#\\+INPUT_METHOD:[ \t]+\\(.*\\)"
	       nil t)
	  (message (format "Setting input method %s" (match-string 1)))
	  (ignore-errors
	    (set-input-method (match-string 1))))))))

(when (functionp 'org-crypt-use-before-save-magic)
  (org-crypt-use-before-save-magic))
(set-face-attribute 'org-hide nil :foreground "#3e4446")
(add-hook 'org-mode-hook 'cw:org:org-mode-setup)
(define-key org-mode-map (kbd "C-c E") 'cw:org:toggle-encryption)
