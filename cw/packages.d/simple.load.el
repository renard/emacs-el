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

;; Remap goto-line to show temporary the line number.
;; http://whattheemacsd.com//key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
