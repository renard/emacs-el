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

(define-key term-raw-map (kbd "M-x") 'execute-extended-command)
(define-key term-raw-map (kbd "M-'") 'ido-switch-buffer)
(define-key term-raw-map (kbd "C-y") 'term-paste)
(define-key term-raw-map (kbd "<C-right>") 'cw:term:forward-word)
(define-key term-raw-map (kbd "<C-left>") 'cw:term:backward-word)
(define-key term-raw-map (kbd "C-c C-'") 'cw:term:toggle-line-mode)
(define-key term-mode-map (kbd "C-c C-'") 'cw:term:toggle-line-mode)
(define-key term-raw-map [mouse-2] 'term-mouse-paste)
