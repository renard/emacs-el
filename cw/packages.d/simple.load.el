(line-number-mode 1)
(column-number-mode 1)
(define-key global-map (kbd "C-z") 'undo)

(transient-mark-mode 1)

(defadvice newline
  (before cw:newline activate protect)
  "Delete spaces before going to next line."
  (unless buffer-read-only
    (save-excursion
      (delete-horizontal-space t)
      (end-of-line)
      ;; This is an empty line, delete its spaces
      (when (looking-back "[ \t]+$")
	(delete-horizontal-space)))))
