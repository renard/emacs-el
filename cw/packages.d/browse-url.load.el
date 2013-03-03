(if (running-macosxp)
    (progn
      (defun browse-url-default-macosx-browser (url &optional new-window)
	(interactive (browse-url-interactive-arg "URL: "))
	(if (and new-window (>= emacs-major-version 23))
	    (ns-do-applescript
	     (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
			     "tell application \"Safari\" to activate") url))
	  (start-process (concat "open " url) nil "open" url)))
      (setq browse-url-browser-function 'browse-url-default-macosx-browser))
  (setq
   browse-url-generic-program
   (if (running-macosxp)
       "/Applications/Safari.app/Contents/MacOS/Safari"
     "raise-x-www-browser")
   browse-url-generic-args nil
   browse-url-browser-function 'browse-url-generic))
