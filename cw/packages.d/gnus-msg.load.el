;;;###autoload
(defun cw:gnus-summary-followup-with-original (n &optional force-news)
  "Run `gnus-summary-followup-with-original'. If called with
`current-prefix-arg', set both `message-yank-prefix' and
`message-citation-line-function' to empty string, and remove
lines starting by \"^>\\s-*\"."
  (interactive "P")
  (message "N: %S force-news: %S" n force-news)
  (if current-prefix-arg
      (let ((message-yank-prefix "")
	    (message-citation-line-function nil))
	(gnus-summary-followup-with-original nil force-news)
	(save-excursion
	  (save-match-data
	    (while (re-search-forward "^>\\s-*" nil t)
	      (replace-match "" nil nil)))))
    (gnus-summary-followup-with-original n force-news)))

(setq gnus-gcc-mark-as-read t)


(define-key gnus-summary-mode-map "F" 'cw:gnus-summary-followup-with-original)
