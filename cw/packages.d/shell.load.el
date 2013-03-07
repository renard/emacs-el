(defadvice shell
  (after cw:shell activate)
  "Kill shell buffer when process exit."
  (cw:shell:set-font)
  (when (ignore-errors (get-buffer-process ad-return-value))
    (set-process-sentinel
     (get-buffer-process ad-return-value)
     (lambda (proc change)
       (when (eq (process-status proc) 'exit)
	 (kill-buffer (process-buffer proc)))))))