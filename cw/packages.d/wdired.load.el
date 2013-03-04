(defadvice wdired-exit (around cw:wdired-exit activate)
  "Keep activated region if defined."
  (let (deactivate-mark)
    ad-do-it))

(define-key wdired-mode-map (kbd "C-o") 'cw:dired-do-shell-mac-open)
