;;;###autoload
(defun diff-buffer-with-its-file()
  "Run `diff-buffer-with-file' using buffer file instead of prompting."
  (interactive)
  (if buffer-file-name
    (diff-buffer-with-file (current-buffer))
    (error "Buffer %s has no associated file on disc." (buffer-name))))

(set-face-attribute 'diff-file-header nil :background nil)
(set-face-attribute 'diff-function nil :background nil)
(set-face-attribute 'diff-header nil
		    :foreground "#729fcf"
		    :background  nil)
(set-face-attribute 'diff-hunk-header nil
		    :foreground "#edd400"
		    :background nil)
(set-face-attribute 'diff-refine-change nil :background nil)
(set-face-attribute 'diff-changed nil :background nil)
(set-face-attribute 'diff-added nil :foreground "#8ae234")
(set-face-attribute 'diff-removed nil :foreground "#f57900")
(set-face-attribute 'diff-index nil :background "#32383a")
