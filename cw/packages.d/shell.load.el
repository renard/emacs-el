(defun cw:shell ()
  "Open `shell' in `default-directory'"
  (interactive)
  (let ((default-directory
	  (or (ignore-errors (dired-current-directory))
	      (ignore-errors (file-name-directory (buffer-file-name)))
	      default-directory)))
    (shell)
    (rename-buffer "*shell*" t)
    (set-process-sentinel
     (get-buffer-process (current-buffer))
     (lambda (proc change)
       (when (eq (process-status proc) 'exit)
	 (kill-buffer (process-buffer proc)))))))

