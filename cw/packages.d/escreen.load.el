;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
	(emphased ""))

    (dolist (s escreens)
      (setq emphased
	    (concat emphased (if (= escreen-current-screen-number s)
				 (propertize (number-to-string s)
					     ;;'face 'custom-variable-tag) " ")
					     ;; 'face 'info-title-3)
					     'face 'font-lock-warning-face)
			       (number-to-string s))
		    " ")))
    (message "escreen: active screens: %s" emphased)))


(defun cw:escreen:check-recursion()
  "Check `recursion-depth'. Prevent from changing screen if
greater that 0."
  (when (> (recursion-depth) 0)
    (error "Recursive edit detected. Quit with C-] before changing escreen.")))

(add-hook 'escreen-goto-screen-before-hook 'cw:escreen:check-recursion)
(add-hook 'escreen-goto-screen-hook
	  'escreen-get-active-screen-numbers-with-emphasis)

(global-set-key (kbd "M-[") 'escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'escreen-goto-next-screen)
(define-key escreen-map "l" 'escreen-get-active-screen-numbers-with-emphasis)
