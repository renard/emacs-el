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

(defadvice escreen-goto-last-screen
  (after cw:goto-last-screen activate)
  "Show the escreen list each time we go to last screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-prev-screen
  (after cw:goto-prev-screen activate)
  "Show the escreen list each time we go to previous screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-next-screen
  (after cw:goto-next-screen activate)
  "Show the escreen list each time we go to next screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-create-screen
  (after cw:create-screen activate)
  "Show the escreen list each time we create a new screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(global-set-key (kbd "M-[") 'escreen-goto-prev-screen)
(global-set-key (kbd "M-]") 'escreen-goto-next-screen)
(define-key escreen-map "l" 'escreen-get-active-screen-numbers-with-emphasis)
