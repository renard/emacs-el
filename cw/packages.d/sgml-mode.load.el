(unless (featurep 'hl-tags-mode)
  (when (require 'hl-tags-mode nil t)
    (add-hook 'html-mode-hook 'hl-tags-mode)))
