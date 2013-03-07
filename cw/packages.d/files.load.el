;;;###autoload
(defun cw:files:make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
	 (save-restriction
	   (widen)
	   (goto-char (point-min))
	   (save-match-data
	     (looking-at "^#!"))))
       (let ((current-mode (file-modes (buffer-file-name)))
	     (add-mode (logand ?\111 (default-file-modes))))
	 (or (/= (logand ?\111 current-mode) 0)
	     (zerop add-mode)
	     (set-file-modes (buffer-file-name)
			     (logior current-mode add-mode))))))

(setq
 backup-directory-alist `((".*" . ,(concat cw:tmp-dir "backup"))))

(add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(add-hook 'after-save-hook 'cw:files:make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'find-file-hook
	  (lambda()
	    (when (functionp 'cw:yasnippet:insert-maybe)
	      (cw:yasnippet:insert-maybe))))
