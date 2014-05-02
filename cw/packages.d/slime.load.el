(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)))

(setq inferior-lisp-program
      (loop for (p o) in
	    '(("/usr/local/bin/sbcl" "")
	      ("/usr/local/bin/ccl64" " -K utf-8")
	      ("/usr/bin/ccl" " -K utf-8")
	      ("~/src/ccl/lx86cl64" " -K utf-8")
	      ("/usr/local/bin/clisp" " -norc -ansi -q -E utf-8"))
	    until (file-exists-p p)
	    finally return (concat p o)))


(defun slime-sbcl ()
  "Run sbcl in slime"
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/sbcl"))
    (slime)))

(defun slime-ccl64 ()
  "Run sbcl in slime"
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/ccl64 -K utf-8"))
    (slime)))


(define-key slime-parent-map (kbd "M-?") 'slime-documentation)
