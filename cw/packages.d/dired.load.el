;;;###autoload
(defun cw:dired ()
  "Open `default-directory' in `dired' without confirmation."
  (interactive)
  (dired default-directory))

(defadvice dired-find-file (around cw:dired-find-file activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let* ((orig (current-buffer))
	 (filename (dired-get-filename t t))
	 (bye-p (file-directory-p filename)))
    ad-do-it
    (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
      (kill-buffer orig))))

(defadvice dired-up-directory (around cw:dired-up-directory activate)
  "Replace current buffer with parent dir."
  (let* ((orig (current-buffer))
	 (parent-dir (file-name-directory
		      (directory-file-name (dired-current-directory)))))
    (when (or
	   (loop for b in (buffer-list)
		 when (with-current-buffer b
			(and (eq major-mode 'dired-mode)
			     (string=  (dired-current-directory) parent-dir)))
		 collect (switch-to-buffer b))
	   ad-do-it)
      (kill-buffer orig))))

(defadvice dired-toggle-read-only (around cw:dired-toggle-read-only activate)
  "Keep activated region if defined."
  (let (deactivate-mark)
    ad-do-it))

(when (running-macosxp)

  (require 'spotlight-comment nil t)
  
  (defun cw:dired-do-open-dir-in-finder ()
    "Open buffer directory  \"open\" on MacOSX."
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "open" current-prefix-arg (list (dired-current-directory)))))
  (define-key dired-mode-map (kbd "C-c C-f") 'cw:dired-do-open-dir-in-finder)

  (defun cw:dired-do-shell-mac-open ()
    "Open file at point using \"open\" on MacOSX."
    (interactive)
    (save-window-excursion
      (dired-do-async-shell-command
       "open" current-prefix-arg
       (dired-get-marked-files t current-prefix-arg))))
  (define-key dired-mode-map (kbd "C-o") 'cw:dired-do-shell-mac-open)

  (defun cw:dired-do-shell-mac-quicklook ()
    "Quicklook file at point using \"qlmanage\" on MacOSX."
    (interactive)
    (let ((filename (dired-file-name-at-point)))
      (when filename
	(let* ((cmd-line (list (executable-find "qlmanage")
			       "-p" filename))
	       (cmd-buf (get-buffer-create (format "QuickLook %s" filename)))
	       (proc (apply 'start-process (car cmd-line)
			    cmd-buf (car cmd-line) (cdr cmd-line))))
	  (process-put proc :cmd-buf cmd-buf)
	  (set-process-sentinel
	   proc
	   (lambda (proc change)
	     (kill-buffer (process-get proc :cmd-buf))))))))
  (define-key dired-mode-map (kbd "S-SPC") 'cw:dired-do-shell-mac-quicklook))

(define-key dired-mode-map (kbd "<return>") 'dired-find-file)
(define-key dired-mode-map (kbd "<C-return>")  'gnus-dired-find-file-mailcap)
(define-key dired-mode-map "/" 'dired-details-toggle)
(define-key dired-mode-map "Y"  'dired-do-relsymlink)
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
(define-key dired-mode-map (kbd "C-c S") 'dired-do-sync)
(define-key dired-mode-map (kbd "C-c s") 'dired-do-sync-pool)
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
