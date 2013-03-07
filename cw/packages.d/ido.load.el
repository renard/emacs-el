(setq
 ido-save-directory-list-file (concat cw:tmp-dir "ido-last")
 ido-enable-flex-matching t
 ido-use-filename-at-point 'guess
 ido-show-dot-for-dired t
 ido-use-url-at-point t
 ido-auto-merge-delay-time 5.00
 ido-default-buffer-method 'selected-window)

(defadvice ido-completion-help (around cw:ido-completion-help activate)
  "Do not create \"Ido Completion Help\" buffer except if
  explicitly called by `cw:ido-completion-help-orig'."
  (when (eq this-command 'cw:ido-completion-help-orig)
    ad-do-it))

(defun cw:ido-completion-help-orig ()
  "Call original `ido-completion-help' function."
  (interactive)
  (ido-completion-help))

(defun cw:ido-init-keys ()
  "Add some usefull ido bindings."
  (define-key ido-common-completion-map (kbd "?") 'cw:ido-completion-help-orig))
(add-hook 'ido-setup-hook 'cw:ido-init-keys)
