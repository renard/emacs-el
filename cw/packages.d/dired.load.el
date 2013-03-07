;;;###autoload
(defun cw:dired ()
  "Open `default-directory' in `dired' without confirmation."
  (interactive)
  (dired default-directory))
