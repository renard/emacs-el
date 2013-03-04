;;;###autoload
(defun cw:gnus:archive-message (current-folder)
  "Post a copy of sent message in current or default folder as
given by `cw:gnus:host-configuration'."
  (message (format "Archiving mail to %s" current-folder))
  (cond
   ((string-equal "" current-folder)
    (plist-get
     (cdr (assoc (intern
		  ;; retrieve the hostname
		  (car (split-string (system-name) "[.]" t)))
		 cw:gnus:host-configuration))
     :archive))
   (t current-folder)))

(defadvice gnus (around cw:gnus activate)
  "Switch to gnus buffer or run `gnus'."
  (when (functionp 'escreen-goto-screen)
    (ignore-errors (escreen-goto-screen 0)))
  (if (buffer-live-p "*Group*")
      (switch-to-buffer"*Group*")
    ad-do-it))
(setq
 gnus-message-archive-group 'cw:gnus:archive-message)
