;; cw-gnus.el --- Gnus configuration

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2011-07-13
;; Last changed: 2011-07-13 22:17:46
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;

;;(require 'gnus-msg)
;;(require 'nnimap)

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
	



;;(provide 'cw-gnus)
