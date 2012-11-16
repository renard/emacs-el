;;; cw-local-prior.el --- Things to be load befor others

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2012-11-05
;; Last changed: 2012-11-05 00:48:11
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

;; http://lists.gnu.org/archive/html/emacs-orgmode/2011-01/msg00978.html
(setq org-export-latex-emphasis-alist 
      '(("*" "\\textbf{%s}" nil)
	("/" "\\emph{%s}" nil)
	("_" "\\underline{%s}" nil)
	("+" "\\texttt{%s}" nil)
	("=" "\\verb=%s=" nil)
	("~" "\\verb~%s~" t)
	("@" "\\alert{%s}" nil))
      org-emphasis-alist
      '(("*" bold "<b>" "</b>")
	("/" italic "<i>" "</i>")
	("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
	("=" org-code "<code>" "</code>" verbatim)
	("~" org-verbatim "<code>" "</code>" verbatim)
	("+" (:strike-through t) "<del>" "</del>")
	("@" org-warning "<b>" "</b>")))



(provide 'cw-local-prior)

;; cw-local-prior.el ends here
