(eval-when-compile
  (require 'man nil t))

(defvar cw:adoc-mode:makefile
  (concat user-emacs-directory "Makefiles/asciidoc.mak")
  "Where to find the asciidoc.mak Makefile")

;;;###autoload
(defun cw:adoc-mode:compile ()
    "Compile manpage and display it in another buffer."
    (interactive)
    (unless (string-match "\\.[1-9]\\.txt$" (buffer-file-name))
      (error "%s does not look like an asciidoc manpage source."
	     (buffer-file-name)))
    (save-buffer)
    ;; on MacOSX XML_CATALOG_FILES is not exported by default.
    (when (running-macosxp)
      (setenv "XML_CATALOG_FILES" "/usr/local/etc/xml/catalog"))
    (let* ((default-directory (file-name-directory (buffer-file-name)))
	   (file (file-name-nondirectory
		  (file-name-sans-extension (buffer-file-name))))
	   (cmd-buf (generate-new-buffer
		     (format " *Make man %s*" file)))
           (proc (start-process "make" cmd-buf
				"make" "-k" "-f" cw:adoc-mode:makefile file)))
      (process-put proc :cmd-buf cmd-buf)
      (process-put proc :file file)
      (set-process-sentinel
       proc
       (lambda(proc status)
	 (when (eq (process-status proc) 'exit)
	   (let ((status (process-exit-status proc))
		 (cmd-buf (process-get proc :cmd-buf))
		 (file (process-get proc :file)))
	     (if (not (eq 0 status))
		 (progn
		   (set-window-buffer (selected-window) cmd-buf)
		   (error "ERROR"))
	       (kill-buffer cmd-buf)
	       (Man-getpage-in-background (concat default-directory file)))))))
      t))

(defun cw:adoc-mode-setup ()
  "Setup `adoc-mode'."
  (setq time-stamp-start "^:date: ")
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^:lang:*\s-*\\(.+\\)" nil t 1)
	(let ((lang (match-string 1)))
	  (flyspell-mode)
	  (ispell-change-dictionary
	   (cond
	    ((string= "fr" lang) "francais")
	    (t "american"))))))))

(define-key adoc-mode-map (kbd "C-c m") 'cw:adoc-mode:compile)
(set-face-attribute 'adoc-hide-delimiter nil :height 1.0)
