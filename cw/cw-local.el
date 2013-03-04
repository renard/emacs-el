;; Init

(let ((generated-autoload-file (concat cw:tmp-dir "autoload")))
  (unless noninteractive
    (update-autoloads-from-directories
     cw:home-dir cw:packages-config-dir
     cw:private-home-dir cw:private-packages-config-dir))
  (when (file-exists-p generated-autoload-file)
    (load generated-autoload-file)))


(defun cw:init()
  "Initialize all settings"
  (color-theme-tango)
  (windmove-default-keybindings)

  ;;
  ;; Key bindings
  ;;

  ;; descbinds-anything
  (global-set-key (kbd "C-h b") 'descbinds-anything)
  ;; dired
  (define-key global-map (kbd "C-x C-d") 'dired)
  ;; frame
  (global-set-key (kbd "C-x K") 'delete-frame)
  ;; hippie-exp
  (define-key global-map (kbd "M-/") 'hippie-expand)
  ;; ido
  (define-key global-map (kbd "C-x C-b") 'ido-switch-buffer)
  (define-key global-map (kbd "C-x b") 'ido-switch-buffer)
  ;; magit
  (global-set-key (kbd "C-x C-z") 'magit-status)
  ;; smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "ESC M-x") 'execute-extended-command)


  (when (running-macosxp)
    (global-set-key (kbd "<C-M-return>") 'ns-toggle-fullscreen)
    (global-set-key (kbd "M-v") 'yank))

  (desktop-save-mode 1)
  (savehist-mode 1)
  (ido-mode t)
  (show-paren-mode t)

  (qbs-init)



)

(unless noninteractive (cw:init))

(provide 'cw-local)
