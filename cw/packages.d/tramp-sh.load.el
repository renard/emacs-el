;; Reload `tramp-compute-multi-hops' to make `cw:tramp-error' advice
;; work. WHY ????"
;; (let ((buffer (find-library "tramp-sh")))
;;   (find-function 'tramp-compute-multi-hops)
;;   (forward-sexp)
;;   (eval-last-sexp nil)
;;   (kill-buffer buffer))

(defadvice tramp-open-connection-setup-interactive-shell
    (before cw:tramp-open-connection-setup-interactive-shell activate)
  "Add process-sentinel to tramp-shells. Kill buffer when process died."
  (set-process-sentinel
   ;; Arg 0 is proc
   (ad-get-arg 0)
   (lambda (proc change)
     (when (eq (process-status proc) 'exit)
       (kill-buffer (process-buffer proc))))))
