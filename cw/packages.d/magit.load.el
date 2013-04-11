(setq
 magit-commit-signoff t
 magit-save-some-buffers nil)

(define-key magit-mode-map "G" 'magit-grep)
(define-key magit-log-edit-mode-map (kbd "C-c C-p") 'magit-identities-change)

(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'magit-log-edit-mode-hook 'magit-identities-set-id)


(set-face-attribute 'magit-item-highlight nil :background nil)
(set-face-attribute 'magit-diff-file-header nil :background nil)
(set-face-attribute 'magit-branch nil :foreground "#729fcf")
(set-face-attribute 'magit-diff-add nil :foreground "#8ae234")
(set-face-attribute 'magit-diff-del nil :foreground "#f57900")
(set-face-attribute 'magit-diff-hunk-header nil :foreground "#fce94f")
(set-face-attribute 'magit-diff-file-header nil :foreground "#ad7fa8")
(set-face-attribute 'magit-item-highlight nil :background  "#32383a")
