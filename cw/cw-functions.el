;;; cw-functions.el --- Some useful functions that don't go anywhere else.

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-03-05
;; Last changed: 2014-03-25 10:28:58
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl))

;; http://www.emacswiki.org/emacs/ParenthesisMatching
;;;###autoload
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;###autoload
(defun set-hooks-debug (&optional desactivate)
  "Toggle hook name display when run.

If universal argument or DESACTIVATE is provided, debug is set to
off."
  (interactive "P")
  (if desactivate
      (progn
	(ad-unadvise 'run-hooks)
	(ad-unadvise 'run-hook-with-args))
    (progn
      (defadvice run-hooks (before cw:run-hooks activate)
	"Show hook name on run."
	(with-current-buffer "*Messages*"
	  (insert (format "running hooks: %s\n" (ad-get-args 0)))))
      (defadvice run-hook-with-args (before cw:run-hook-with-args activate)
	"Show hook name on run."
	(with-current-buffer "*Messages*"
	  (insert (format "running hooks with args: %s\n" (ad-get-args 0))))))))

;;;###autoload
(defun asciify-string (string)
"Convert STRING to ASCII string.
For example:
“passé” becomes “passe”"
  (loop for c across string
	with ret
	if (member (get-char-code-property c 'general-category)
		   '(Lu Ll))
	collect (downcase
		 (char-to-string
		  (or (car (get-char-code-property c 'decomposition))
		      c)))
	into ret
	else
	collect (char-to-string c) into ret
	finally return (mapconcat 'identity ret "")))

;;;###autoload
(defmacro time (form)
  "Evaluate FORM and return its execution time in seconds."
  `(let ((t0 (current-time)))
     ,form
     (time-to-seconds (time-subtract (current-time) t0))))

;;;###autoload
(defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))


;;;###autoload
(defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))

;;;###autoload
(defun sort-lines-by-length (beg end)
  "Sort lines by length in region starting at BEG ending at END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((items (sort
		    (split-string
		     (buffer-substring (point-min) (point-max))
		     "[\n]")
		    (lambda(x y) (< (length x) (length y))))))
	(delete-region (point-min) (point-max))
	(goto-char (point-min))
	(insert (apply 'concat
		       (map 'list
			    (lambda (x) (format "%s\n" x)) items)))))))

(defun cw:note-buffer-create (&optional new)
  "Create a Note buffer. If NEW (or called with
`universal-argument', create a new Note buffer."
  (interactive "P")
  (let ((buffer-name "*Note*"))
    (switch-to-buffer
     (get-buffer-create
      (if current-prefix-arg (generate-new-buffer-name buffer-name)
	buffer-name)))))


(provide 'cw-functions)

;; cw-functions.el ends here
