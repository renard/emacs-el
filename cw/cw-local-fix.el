;;; cw-local-fix.el --- Some hotfix for emacs

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-01-20
;; Last changed: 2012-01-28 00:33:10
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:


(eval-after-load "hideshow"
  '(progn
     ;; Bug submitted http://debbugs.gnu.org/cgi/bugreport.cgi?bug=10496
     (defun hs-hide-all ()
       "Hide all top level blocks, displaying only first and last lines.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.  If
`hs-hide-comments-when-hiding-all' is non-nil, also hide the
comments."
       (interactive)
       (hs-life-goes-on
	(save-excursion
	  (unless hs-allow-nesting
	    (hs-discard-overlays (point-min) (point-max)))
	  (goto-char (point-min))
	  (let ((spew (make-progress-reporter "Hiding all blocks..."
					      (point-min) (point-max)))
		(re (concat "\\("
			    hs-block-start-regexp
                       "\\)"
                       (if hs-hide-comments-when-hiding-all
                           (concat "\\|\\("
                                   hs-c-start-regexp
                                   "\\)")
                         ""))))
	    (while (progn
		     (unless hs-hide-comments-when-hiding-all
		       (forward-comment (point-max)))
		     (re-search-forward re (point-max) t))
	      (if (match-beginning 1)
		  ;; we have found a block beginning
		  (progn
		    (goto-char (match-beginning 1))
		    (unless
			(if hs-hide-all-non-comment-function
			    (funcall hs-hide-all-non-comment-function)
			  (hs-hide-block-at-point t))
		      ;; Go to end of matched data to prevent from getting stuck
		      ;; with an endless loop.
		      (goto-char (match-end 0))))
		;; found a comment, probably
		(let ((c-reg (hs-inside-comment-p)))
		  (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
	      (progress-reporter-update spew (point)))
	    (progress-reporter-done spew)))
	(beginning-of-line)
	(run-hooks 'hs-hide-hook)))))

(eval-after-load "cc-engine"
  '(progn
     (defsubst c-state-pp-to-literal (from to)
       ;; Do a parse-partial-sexp from FROM to TO, returning either
       ;;     (STATE TYPE (BEG . END))     if TO is in a literal; or
       ;;     (STATE)                      otherwise,
       ;; where STATE is the parsing state at TO, TYPE is the type of the literal
       ;; (one of 'c, 'c++, 'string) and (BEG . END) is the boundaries of the literal.
       ;;
       ;; Only elements 3 (in a string), 4 (in a comment), 5 (following a quote),
       ;; 7 (comment type) and 8 (start of comment/string) (and possibly 9) of
       ;; STATE are valid.
       (save-restriction
	 (widen)
	 (save-excursion
	   (let ((s (parse-partial-sexp from to))
		 ty)
	     (when (or (nth 3 s) (nth 4 s))	; in a string or comment
	       (setq ty (cond
			 ((nth 3 s) 'string)
			 ((eq (nth 7 s) t) 'c++)
			 (t 'c)))
	       (parse-partial-sexp (point) (point-max)
				   nil			 ; TARGETDEPTH
				   nil			 ; STOPBEFORE
				   s			 ; OLDSTATE
				   'syntax-table))	 ; stop at end of literal
	     (if ty
		 `(,s ,ty (,(nth 8 s) . ,(point)))
	       `(,s))))))
     ))


(provide 'cw-local-fix)
