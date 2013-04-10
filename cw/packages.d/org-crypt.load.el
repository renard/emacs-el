(unless (featurep 'org-crypt)
  (require 'org-crypt))

(defadvice org-encrypt-entry (around cw:org-encrypt-entry activate)
  "Go to CRYPTKEY property node make sure that a GPG key
would be used if applicable ad remove CLEAR tag.
"
  (search-backward ":CRYPTKEY:" nil t)
  (org-back-to-heading t)
  (show-subtree)
  ad-do-it
  (org-back-to-heading t)
  (org-set-tags-to (delete "CLEAR" (org-get-tags)))
  (hide-entry))

(defadvice org-decrypt-entry (around cw:org-decrypt-entry activate)
  "Add a CLEAR tag to the current entry."
  (org-back-to-heading t)
  (show-subtree)
  (let ((modified-flag (buffer-modified-p)))
    ad-do-it
    (org-back-to-heading t)
    (hide-subtree)
    (let ((tags-list (org-get-tags)))
      (when (member org-crypt-tag-matcher tags-list)
	(org-set-tags-to (append '("CLEAR") tags-list))
	(hide-entry)
	(show-children 3)))
    (set-buffer-modified-p modified-flag))
  (setq org-crypt-disable-auto-save t))

;;;###autoload
(defun cw:org:toggle-encryption()
  "Toggle encryption in for current entry."
  (interactive)
  (org-crypt-use-before-save-magic)
  (save-excursion
    (org-back-to-heading t)
    (org-show-entry)
    (next-line)
    (if (looking-at "-----BEGIN PGP MESSAGE-----")
	(org-decrypt-entry)
      (org-encrypt-entry))))
