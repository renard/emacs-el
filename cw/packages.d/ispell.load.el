(setq ispell-program-name
      (or
       ;; hunspell is the fastest spell checker
       ;;
       ;; If using hunspell, do not forget to link dictionnaries in
       ;; ~/Library/Spelling:
       ;;
       ;; - en_US.{aff,dic}
       ;; - fr.{aff,dic}
       (locate-file "hunspell"   exec-path exec-suffixes 'file-executable-p)
       (locate-file "ispell"   exec-path exec-suffixes 'file-executable-p)
       (locate-file "aspell"   exec-path exec-suffixes 'file-executable-p)))

(setq cw:ispell:program (intern (file-name-nondirectory ispell-program-name)))

(setq ispell-extra-args '("--sug-mode=ultra"))
(cond
 ((eq cw:ispell:program 'hunspell)
  ;; Use en_US as default directory
  (setq ispell-dictionary "en_US")))


;; Create dictionnary list.
(loop for lang in '(;;(nil ("-d" "en_US") "default")
		    ("american" ("-d" "en_US") "en_US")
		    ("francais" ("-d" "fr") "fr")
		    ("francais7" ("-d" "fr") nil)
		    ("francais-tex" ("-d" "fr") nil))
      do (destructuring-bind (name args alias) lang
	   (let ((dict (assoc name ispell-dictionary-base-alist)))
	     (setf (nth 4 dict) args)
	     ;; (add-to-list 'ispell-dictionary-alist dict)
	     (when alias
	       (setf (nth 0 dict) alias)
	       (add-to-list 'ispell-dictionary-alist dict))
		 )))

(setq ispell-local-dictionary-alist ispell-dictionary-alist)
