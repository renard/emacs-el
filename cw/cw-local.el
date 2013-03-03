;; Init
(defun cw:init()
  "Initialize all settings"
  (color-theme-tango)
  (windmove-default-keybindings)

  ;;
  ;; Key bindings
  ;;

  ;; magit
  (global-set-key (kbd "C-x C-z") 'magit-status)
  ;; smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "ESC M-x") 'execute-extended-command)


)

(unless noninteractive (cw:init))

(provide 'cw-local)
