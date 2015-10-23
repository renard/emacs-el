;;;###autoload
(defun cw:build-latex(&optional rounds file)
  "Compile FILE or local buffer or an asked file asynchronously
using `tex-command' ROUNDS times.

If run using `prefix-argument' the document will be processed 3
time.

`process-environment' can be changed using lines such as:

  %+ENV: VAR=VAL
  %+LATEX: /path/to/tex-processor

xelatex is used as default tex-processor.
"
  (interactive "P")
  (let* ((file (expand-file-name
		(or
		 file
		 (when (eq major-mode 'latex-mode) (buffer-file-name))
		 (read-file-name "Publish LaTeX from: " nil nil t))))
	 (rounds (cond
		  ((equal '(4) rounds) 2)
		  ((equal '(16) rounds) 3)
		  ((member rounds '(1 2 3)) rounds)
		  (t 1)))
	 (exec-path (append '("/Users/renard/bin") exec-path))
	 (process-environment
	  (append process-environment
		  (with-temp-buffer
		    (insert-file-contents file)
		    	 (goto-char (point-min))
			 (save-match-data
			   (loop for m = (search-forward-regexp
					  "^\\s-*%\\+ENV:\\s-*\\(.+\\)$"
					  nil t)
				 while m
				 collect (match-string 1))))))
	 (latex-bin (or (save-excursion
			  (save-match-data
			    (goto-char (point-min))
			    (when
				(search-forward-regexp
				 "^\\s-*%\\+LATEX:\\s-*\\(.+\\)$" nil t)
			      (executable-find (match-string 1)))))
			(executable-find "xelatex")))
	 (default-directory (file-name-directory file))
	 (cmd-line (list
		    latex-bin "-interaction" "nonstopmode"
		    "-shell-escape" "-output-directory" "." file))
	 (cmd-buf-name (format "*Building %s*"
			       (file-name-nondirectory file)))
	 (cmd-buf (get-buffer-create cmd-buf-name))
	 (proc (apply 'start-process (car cmd-line) cmd-buf
		      (car cmd-line) (cdr cmd-line)))

	 (proc-sentinel
	  (lambda (proc change)
	    (when (eq (process-status proc) 'exit)
	      (let ((status  (process-exit-status proc))
		    (proc-buf (process-buffer proc))
		    (file (process-get proc :file))
		    (rounds (1- (process-get proc :rounds))))
		(message "Status: %S" status)
		(if (not (eq 0 status))
		    (progn
		      (when proc-buf
			(set-window-buffer (selected-window) proc-buf))
		      (error "Failed to build %s" file))

		  (when proc-buf (kill-buffer proc-buf))
		  (if (> rounds 0)
		      (cw:build-latex rounds file)
		    (let ((f (file-name-sans-extension file)))
		      (loop for e in '("aux" "log" "nav" "out" "pyg" "snm" "toc")
			    for fe = (format "%s.%s" f e)
			    when (file-exists-p fe)
			    do (delete-file fe))
		      (shell-command (format "open \"%s.pdf\"" f)))
		    (message  "Built %s" file))))))))

    (message "Running: %S"
	     (mapconcat 'shell-quote-argument
			cmd-line " "))
    (process-put proc :file file)
    (process-put proc :rounds rounds)
    (set-process-sentinel proc proc-sentinel)
    (message "Building %s (%d round%s left)"
	     file rounds (if (> rounds 1) "s" ""))))
