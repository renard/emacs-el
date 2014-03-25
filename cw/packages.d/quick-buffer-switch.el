;; (setq qbs-predicates-plist nil)
;; (qbs-init)
(qbs-add-predicates
 (make-qbs:predicate
  :name 'notes
    :description "Show all Notes buffers (name starting with *Note*)."
    :shortcut "C-n"
    :test '(when (string-match "^\*Note\*" qbs:buffer-name)
	     qbs:buffer-name)))

