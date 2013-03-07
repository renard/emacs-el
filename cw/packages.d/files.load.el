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

(defvar cw:new-file-tpl-maker "new-file-tpl"
  "String to be inserted as new file skeleton by
  `cw:files:insert-template-maybe'.")

;;;###autoload
(defun cw:files:insert-template-maybe ()
  "Insert a skeleton if `cw:new-file-tpl-maker' is found as a
`yasnippet'."
  (when
      (and
       (not noninteractive)
       (buffer-file-name)
       (not (file-exists-p (buffer-file-name)))
       (= (point-max) 1))
    (unless (functionp 'yas-expand)
      (require 'yasnippet))
    (let ((mode (symbol-name major-mode)))
      (loop for d in yas-snippet-dirs
	    when (file-exists-p
		  (format "%s/%s/%s.yasnippet" d mode
			  cw:new-file-tpl-maker))
	    do (progn
		 ;;(message "Found: %s/%s/%s.yasnippet" d mode cw:new-file-tpl-maker)
		 (insert cw:new-file-tpl-maker)
		 ;; reload templates before expansion
		 ;; why is this needed?
		 (yas-reload-all)
		 (yas-expand))
	    and return t))))

(setq
 backup-directory-alist `((".*" . ,(concat cw:tmp-dir "backup"))))

(add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

(add-hook 'after-save-hook 'cw:files:make-buffer-file-executable-if-script-p)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'find-file-hook 'cw:files:insert-template-maybe)
