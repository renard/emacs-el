(setq
 backup-directory-alist `((".*" . ,(concat cw:tmp-dir "backup"))))

(add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

