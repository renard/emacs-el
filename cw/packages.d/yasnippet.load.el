(defvar cw:yasnippet:in-expansionp nil
  "Define if snippet is in expansion phase or not.")
(make-local-variable 'cw:yasnippet:in-expansionp)

(defun cw:yasnippet:toggle-expansion-semaphore ()
  "Toggles `cw:yasnippet:in-expansionp'."
  (setq cw:yasnippet:in-expansionp (not cw:yasnippet:in-expansionp)))

(add-hook 'yas/after-exit-snippet-hook 'cw:yasnippet:toggle-expansion-semaphore)
(add-hook 'yas/before-expand-snippet-hook 'cw:yasnippet:toggle-expansion-semaphore)

(setq-default yas/dont-activate 'cw:yasnippet:do-activatep)
(setq yas/trigger-key "M-TAB")
(define-key global-map (kbd "<C-tab>") 'yas/expand)
;;yas/prompt-functions '(yas/completing-prompt))
(add-to-list 'yas/snippet-dirs "~/.emacs.d/templates")
(yas/reload-all)
(add-hook 'yas/minor-mode-hook 'cw:yasnippet:insert-snippet-new-file)
