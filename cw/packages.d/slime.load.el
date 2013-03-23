(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)))

(setq inferior-lisp-program
      (loop for p in '("/usr/local/bin/ccl64"
		       "/usr/bin/ccl"
		       "~/src/ccl/lx86cl64")
	    until (file-exists-p p)
	    finally return (concat p " -K utf-8")))

