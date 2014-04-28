(unless (boundp 'term-raw-map) (require 'term nil t))
(when (require 'escreen nil t)
  (define-key term-raw-map escreen-prefix-char escreen-map)
  (define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
  (define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen))

(set-face-attribute 'term-color-black nil :foreground "black")
(set-face-attribute 'term-color-red nil :foreground "#f57900")
(set-face-attribute 'term-color-green nil :foreground "#8ae234")
(set-face-attribute 'term-color-yellow nil :foreground "#edd400")
(set-face-attribute 'term-color-blue nil :foreground "#729fcf")
(set-face-attribute 'term-color-magenta nil :foreground "#ad7fa8")
(set-face-attribute 'term-color-cyan nil :foreground "cyan3")
(set-face-attribute 'term-color-white nil :foreground "#eeeeec")

(setq
 term-default-fg-color "#eeeeec"
 term-default-bg-color 'unspecified)

(add-hook 'term-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'term-mode-hook 'cw:shell:set-font)

(defun cw:term:toggle-line-mode()
  "Toogle between line and char mode in term-mode."
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (end-of-buffer)
    (term-char-mode)))

(defadvice global-hl-line-highlight
  (around cw:global-hl-line-highlight activate)
  "Disable hl-line-highlight in `term-char-mode'."
  (unless (and (eq major-mode 'term-mode) (term-in-char-mode))
    ad-do-it))

(defun cw:term:backward-word ()
  "Move backward work in term-mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun cw:term:forward-word ()
  "Move forward work in term-mode."
  (interactive)
  (term-send-raw-string "\ef"))

(defun cw:shell:set-font()
  "Set default font for terminal."
  (set
   (make-local-variable 'buffer-face-mode-face)
   (if (running-macosxp)
       '(:family "Monaco" :height 120)
   '(:family "Terminus" :height 120)))
  (buffer-face-mode))

;;;###autoload
(defun cw:term (&optional sudo)
  "Run `term' with specific settings."
  "Run terminal in current buffer directory."
  (interactive "P")
  (cw:with-parse-directory
   sudo
   (progn
     (unless (boundp 'explicit-shell-file-name) (require 'term nil t))
     (let ((shell (or explicit-shell-file-name
		      (getenv "ESHELL")
		      (getenv "SHELL")
		      "/bin/sh"))
	   (sh-cmd)
	   (cmd ""))
       ;; Create shell argument to connect to remote host.
       (when host (setq sh-cmd (concat sh-cmd (format "ssh %s -t" host))))
       (when user (setq sh-cmd (concat sh-cmd (format " sudo -u %s -s" user))))
       (when sh-cmd (setq sh-cmd `("-c" ,sh-cmd)))

       ;; Prepare commands to change directory
       (when localname (setq cmd (format "%s cd %s;" cmd localname)))
       (when host
	 (setq cmd (concat cmd " "
			   ;; enable remote directory tracking
			   "function prompt_cmd { "
			   "echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)};"
			   "echo -e \"\\033AnSiTc\" $(pwd);"
			   "echo -e \"\\033AnSiTh\" ${TRAMP_HOSTNAME-" host "};"
			   " };"
			   "export PROMPT_COMMAND=prompt_cmd;")))
       (setq cmd (concat cmd " "
			 ;; don't store these lines into shell history
			 "history -d $((HISTCMD - 1)); clear; \n"))
       ;; Start shell
       (switch-to-buffer
	(apply  'make-term
	 (append
	  (list
	   (if host (concat "ssh " host)
	     (concat (file-name-nondirectory shell) " " default-directory))
	   shell nil)
	  sh-cmd)))

       (set-process-sentinel
	(get-buffer-process (current-buffer))
	(lambda (proc change)
	  (when (eq (process-status proc) 'exit)
	    (kill-buffer (process-buffer proc)))))

       (term-mode)
       (term-char-mode)
       (process-send-string (get-buffer-process (current-buffer)) cmd)))))

(define-key term-raw-map (kbd "M-x") 'execute-extended-command)
(define-key term-raw-map (kbd "M-'") 'ido-switch-buffer)
(define-key term-raw-map (kbd "C-y") 'term-paste)
(define-key term-raw-map (kbd "<C-right>") 'cw:term:forward-word)
(define-key term-raw-map (kbd "<C-left>") 'cw:term:backward-word)
(define-key term-raw-map (kbd "C-c C-'") 'cw:term:toggle-line-mode)
(define-key term-mode-map (kbd "C-c C-'") 'cw:term:toggle-line-mode)
(define-key term-raw-map [mouse-2] 'term-mouse-paste)
