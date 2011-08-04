;; init.el --- emacs local configuration

;; Copyright © 2011 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2011-08-03 18:54:40
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;

(unless noninteractive
  (find-file "~/.emacs.d/.tmp/autoload")
  (erase-buffer)
  (generate-file-autoloads "~/.emacs.d/cw/cw-functions.el")
  (save-buffer)
  (kill-buffer (current-buffer))
  ;; Other autoloads
  (autoload 'org-crypt-use-before-save-magic "org-crypt"))

(load "~/.emacs.d/.tmp/autoload")

 ;; a
(eval-after-load 'adoc-mode
  '(progn
     (add-hook 'adoc-mode-hook 'flyspell-mode)
     (add-hook 'adoc-mode-hook 'flyspell-buffer)
     (define-key adoc-mode-map (kbd "C-c m") 'cw:adoc-mode:compile)))

(eval-after-load 'ansi-color
  '(progn
     (setq ansi-color-names-vector
	   ["black" "#f57900" "#8ae234" "#edd400" "#729fcf"
	    "#ad7fa8" "cyan3" "#eeeeec"]
	   ansi-color-map (ansi-color-make-color-map))))

 ;; b
(eval-after-load 'browse-url
  '(progn
     (setq
      browse-url-generic-program "x-www-browser"
      browse-url-browser-function 'browse-url-generic)))

(eval-after-load 'buffer-move
  '(progn
     (global-set-key (kbd "<C-S-up>") 'buf-move-up)
     (global-set-key (kbd "<C-S-down>") 'buf-move-down)
     (global-set-key (kbd "<C-S-left>") 'buf-move-left)
     (global-set-key (kbd "<C-S-right>") 'buf-move-right)))

 ;; c
(eval-after-load "comint"
  '(progn
     (set-face-attribute 'comint-highlight-input nil :italic nil)))

(eval-after-load "color-theme"
  '(progn
     (color-theme-tango)))

(eval-after-load "color-theme-tango"
  '(progn
     (set-face-attribute 'font-lock-string-face nil :italic nil)
     (set-face-attribute 'font-lock-function-name-face nil :italic nil)))

(eval-after-load 'common-win
  '(progn
     (setq
      x-select-enable-clipboard t)))

 ;; d
(eval-after-load 'desktop
  '(progn
     (setq 
      desktop-restore-eager 20)))

(eval-after-load 'diff-mode
  '(progn
     (message "Setting font attribute for diff-mode")
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
     (set-face-attribute 'diff-index nil :background "#32383a")))

(eval-after-load 'dired
  '(progn

     (defadvice dired-find-file (around cw:dired-find-file activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
	      (filename (dired-get-filename t t))
	      (bye-p (file-directory-p filename)))
	 ad-do-it
	 (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
	   (kill-buffer orig))))

     (defadvice dired-up-directory (around cw:dired-up-directory activate)
       "Replace current buffer with parent dir."
       (let* ((orig (current-buffer)))
	 ad-do-it
	 (kill-buffer orig)))

     (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

     (define-key dired-mode-map (kbd "<C-return>")  'gnus-dired-find-file-mailcap)
     (define-key dired-mode-map "/" 'dired-details-toggle)
     (define-key dired-mode-map "Y"  'dired-do-relsymlink)
     (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
     (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync)
     (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)))

 ;; e
(eval-after-load 'erc
  '(progn
     (setq 
      erc-server-auto-reconnect nil
      erc-kill-server-buffer-on-quit t)
     (set-face-attribute 'erc-notice-face nil :foreground "#ad7fa8")
     (set-face-attribute 'erc-input-face nil :foreground "#babdb6")
     (set-face-attribute 'erc-my-nick-face nil :foreground "#729fcf")
     (define-key erc-mode-map (kbd "C-x k") 'erc-iswitchb)
     (define-key erc-mode-map (kbd "C-C C-\\") 'cw:erc:channel-next-modified)
     (define-key erc-mode-map (kbd "C-C C-]") 'cw:erc:channel-next)
     (define-key erc-mode-map (kbd "C-C C-[") 'cw:erc:channel-prev)))

(eval-after-load 'erc-track
  '(progn
     (setq 
      erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "333" "353")
      erc-track-use-faces t
      erc-track-priority-faces-only 'all)))

(eval-after-load 'escreen
  '(progn

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
     (unless noninteractive
       (global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis))))

 ;; f
(eval-after-load 'faces
  '(progn
     (unless noninteractive
       (set-face-font 'default "DejaVu Sans Mono-10"))))

(eval-after-load 'files
  '(progn
     (add-hook 'after-save-hook 'cw:make-buffer-file-executable-if-script-p)
     (add-hook 'before-save-hook 'time-stamp)))

(eval-after-load 'flyspell
  '(progn
     (define-key flyspell-mode-map (kbd "C-;") nil)))

 ;; g
(eval-after-load 'gnus
  '(progn
     (defadvice gnus (around cw:gnus activate)
       "Switch to gnus buffer or run `gnus'."
       (when (functionp 'escreen-goto-screen)
	 (ignore-errors (escreen-goto-screen 0)))
       (if (buffer-live-p "*Group*")
	   (switch-to-buffer"*Group*")
	 ad-do-it))
     (setq       
      gnus-message-archive-group 'cw:gnus:archive-message)))

(eval-after-load 'gnus-art
  '(progn
     (setq 
      gnus-visible-headers (concat gnus-visible-headers
				   "\\|^User-Agent:\\|^X-Mailer:")
      gnus-article-update-date-headers nil)))

(eval-after-load 'gnus-msg
  '(progn
     (setq
      gnus-gcc-mark-as-read t)))

(eval-after-load 'gnus-start
  '(progn
     (setq gnus-init-file 
	   (concat (file-name-as-directory user-emacs-directory) "cw/cw-gnus.el"))
     (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)))

(eval-after-load 'gnus-sum
  '(progn
     (setq       
      gnus-user-date-format-alist
      '(
	((gnus-seconds-today) .           "Today      %H:%M")
	((+ 86400 (gnus-seconds-today)) . "%Y-%m-%d %H:%M")
	(604800 .                         "%Y-%m-%d %H:%M") ;;that's one week
	((gnus-seconds-month) .           "%Y-%m-%d %H:%M")
	((gnus-seconds-year) .            "%Y-%m-%d %H:%M")
	(t .                              "%Y-%m-%d %H:%M"))
      gnus-summary-line-format (concat "%U%R %5,5k %&user-date; %-20,20n %B%s\n")
      ;;gnus-group-line-format "%M%S%p%P%5uy:%B%(%g%)%O\n"
      ;;gnus-group-line-format "%M%S%p%P:%B%(%g%)%O\n"
      gnus-sum-thread-tree-false-root " ♽ "
      gnus-sum-thread-tree-single-indent "⚙ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-root "⚈ "
      gnus-sum-thread-tree-leaf-with-other "├─►"  ; "┣━► "  "▶"
      gnus-sum-thread-tree-single-leaf     "└─►"  ; "┗━► "
      gnus-sum-thread-tree-vertical        "│"   ; "┆" "┋")  "│" "┆"
      )))

(eval-after-load 'gnus-win
  '(progn
     (gnus-add-configuration
      '(article
	(horizontal 8
		    (group 50)
		    (vertical 1.0
			      (summary 20 point)
			      (article 1.0)))))
     (gnus-add-configuration
      '(summary
	(vertical 1.0
		  (horizontal 1.0
			      (group 50)
			      (summary 1.0 point)
			      (if gnus-carpal
				  '(summary-carpal 4))))))))

 ;; h
(eval-after-load 'hideshow
  '(progn

     (defun display-code-line-counts (ov)
       (when (eq 'code (overlay-get ov 'hs))
	 (overlay-put ov 'display
		      (format "...  / %d"
			      (count-lines (overlay-start ov)
					   (overlay-end ov))))))
     
     (setq hs-set-up-overlay 'display-code-line-counts)


     (define-key hs-minor-mode-map (kbd "C-c C-'") 'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "C-c <C-return>") 'hs-show-all)
     (define-key hs-minor-mode-map (kbd "C-c RET") 'hs-hide-all)
     (add-hook 'hs-minor-mode-hook 'hs-hide-all)))

(eval-after-load 'hippie-exp
  '(progn
     (setq hippie-expand-try-functions-list
	   '(try-expand-dabbrev
	     try-expand-dabbrev-all-buffers
	     try-complete-file-name-partially
	     try-complete-file-name
	     try-expand-all-abbrevs
	     try-expand-list
	     try-expand-line
	     try-expand-dabbrev-from-kill
	     try-complete-lisp-symbol-partially
	     try-complete-lisp-symbol))))

 ;; i
(eval-after-load 'ido
  '(progn
     ;; ido configuration
     (setq
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-show-dot-for-dired t
      ido-use-url-at-point t
      ido-auto-merge-delay-time 2.00)
     ;; Some ido key bindings
     (define-key global-map (kbd "C-x C-b") 'ido-switch-buffer)
     (define-key global-map (kbd "C-x b") 'ido-switch-buffer)
     ;; Do not create "Ido Completion Help" buffer.
     (defadvice ido-completion-help (around cw:ido-completion-help activate))
     (defun cw:ido-init-keys ()
       "Add some usefull ido bindings."
       (define-key ido-common-completion-map (kbd "?") 'ad-Orig-ido-completion-help))
     (add-hook 'ido-setup-hook 'cw:ido-init-keys)))

 ;; l
(eval-after-load 'lisp-mode
  '(progn
     (defun cw:emacs-lisp-mode-setup ()
       "Setup for `emacs-lisp-mode'."
       (when (or (not (boundp 'cw:yasnippet:in-expansionp))
		 (and (boundp 'cw:yasnippet:in-expansionp)
		      (not cw:yasnippet:in-expansionp)))
	 (hs-minor-mode)
	 (flyspell-prog-mode))
       (rainbow-delimiters-mode 1))
     (add-hook 'emacs-lisp-mode-hook 'cw:emacs-lisp-mode-setup)))

 ;; m
(eval-after-load 'magit
  '(progn
     (setq
      magit-commit-signoff t
      magit-save-some-buffers nil)
     (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
     (set-face-attribute 'magit-item-highlight nil :background nil)
     (set-face-attribute 'magit-diff-file-header nil :background nil)
     (set-face-attribute 'magit-branch nil :foreground "#729fcf")
     (set-face-attribute 'magit-diff-add nil :foreground "#8ae234")
     (set-face-attribute 'magit-diff-del nil :foreground "#f57900")
     (set-face-attribute 'magit-diff-hunk-header nil :foreground "#fce94f")
     (set-face-attribute 'magit-diff-file-header nil :foreground "#ad7fa8")
     (set-face-attribute 'magit-item-highlight nil :background  "#32383a")))

(eval-after-load 'mailcap
  '(progn
     (add-to-list 'mailcap-mime-extensions '(".mkv" . "video/x-matroska"))
     (mailcap-parse-mailcaps nil t)))

(eval-after-load 'menu-bar
  '(progn (menu-bar-mode -1)))

(eval-after-load 'message
  '(progn
     (setq message-signature-insert-empty-line t)
     (add-hook 'message-mode-hook 'cw:gnus:configure-group)
     (add-hook 'message-mode-hook 'flyspell-mode)
     (add-hook 'message-mode-hook 'flyspell-buffer)))

(eval-after-load 'minibuffer
  '(progn
     (setq completion-auto-help t)
     (define-key minibuffer-local-completion-map "?" 'minibuffer-completion-help)))

(eval-after-load 'mm-decode
  '(progn
     (setq mm-inline-large-images 'resize)
     (add-to-list 'mm-attachment-override-types "image/.*")))

 ;; o
(eval-after-load 'org
  '(progn
     (defun cw:org:org-mode-setup ()
       "Setup buffer for `org-mode' files."
       (unless (and
		(boundp 'cw:org:publishing-project)
		cw:org:publishing-project)
	 (setq time-stamp-start "^#\\+DATE: ")
	 ;; flyspell mode to spell check everywhere
	 (flyspell-mode 1)
	 (save-excursion
	   (goto-char (point-min))
	   (save-match-data
	     (when (search-forward-regexp
		    "^#\\+LANG:[ \t]+\\(.*\\)"
		    nil t)
	       (message (format "Loading dict %s" (match-string 1)))
	       (ignore-errors
		 (ispell-change-dictionary (match-string 1))))))))
     (when (functionp 'org-crypt-use-before-save-magic)
       (org-crypt-use-before-save-magic))
     (set-face-attribute 'org-hide nil :foreground "#3e4446")
     (add-hook 'org-mode-hook 'cw:org:org-mode-setup)
     (define-key org-mode-map (kbd "C-c E") 'cw:org:toggle-encryption)))

(eval-after-load 'org-contacts
  '(progn 
     (setq
      org-contacts-files
      '("~/.emacs.d/org/contacts.org"))))

(eval-after-load 'org-crypt
  '(progn
     (defadvice org-encrypt-entry
       "Go to CRYPTKEY property node make sure that a GPG key
would be used if applicable ad remove CLEAR tag.
"
       (around cw:org-encrypt-entry activate)
       (search-backward ":CRYPTKEY:" nil t)
       (org-back-to-heading t)
       (show-subtree)
       ad-do-it
       (org-back-to-heading t)
       (org-set-tags-to (delete "CLEAR" (org-get-tags)))
       (hide-entry))
     (defadvice org-decrypt-entry
       "Add a CLEAR tag to the current entry."
       (around cw:org-decrypt-entry activate)
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
       (setq org-crypt-disable-auto-save t)))

(eval-after-load 'org-install
  '(progn
     (defadvice org-publish-projects
       (around cw:org:publish-projects (projects) activate)
       "Set `cw:publishing-project' to `t' when publishing a project."
       (let ((cw:org:publishing-project t))
	 ad-do-it))))


 ;; p
(eval-after-load 'paren
  '(progn
     (setq show-paren-style 'parenthesis)))

 ;; r
(eval-after-load 'rainbow-delimiters
  '(progn
     (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#fce94f" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#729fcf" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#8ae234" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#ad7fa8" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#f57900" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#c4a000" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#3465a4" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#4e9a06" :bold t)
     (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#75507b" :bold t)))


(eval-after-load 'shr-color
  '(progn
     (setq shr-color-visible-luminance-min 75)))

 ;; s
(eval-after-load 'scroll-bar
  '(progn (scroll-bar-mode -1)))

(eval-after-load 'sendmail
  '(progn
     (defadvice mail-envelope-from
       (around cw:mail-envelope-from activate)
       "Get envelope sender from Sender, Return-path, From fields
or `mail-envelope-from'."
       (setq ad-return-value
	     (or
	      (nth 1 (mail-extract-address-components
		      (or 
		       (message-fetch-field "Sender")
		       (message-fetch-field "Return-path")
		       (message-fetch-field "From"))))
	      mail-envelope-from)))
     (setq
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-from-style 'angles)))

(eval-after-load 'smtpmail
  '(progn
     (setq smtpmail-default-smtp-server "127.0.0.1")))

(eval-after-load "subr"
  ;; subr is not provided
  '(progn
     (fset 'yes-or-no-p 'y-or-n-p)))

(eval-after-load 'simple
  '(progn
    (line-number-mode 1)
    (column-number-mode 1)
    (define-key global-map (kbd "C-z") 'undo)

    (transient-mark-mode 1)

    (defadvice newline
      (before cw:newline activate protect)
      "Delete spaces before going to next line."
      (unless buffer-read-only
	(save-excursion
	  (end-of-line)
	  ;; This is an empty line, delete its spaces
	  (when (looking-back "[ \t]+$")
	    (delete-horizontal-space)))))))

(eval-after-load 'shell
  '(progn
     (defadvice shell
       (after cw:shell activate)
       "Kill shell buffer when process exit."
       (cw:shell:set-font)
       (when (ignore-errors (get-buffer-process ad-return-value))
	 (set-process-sentinel
	  (get-buffer-process ad-return-value)
	  (lambda (proc change)
	    (when (eq (process-status proc) 'exit)
	      (kill-buffer (process-buffer proc)))))))))

(eval-after-load 'startup
  '(progn
     (setq inhibit-splash-screen t)))

 ;; t
(eval-after-load 'term
  '(progn
     (define-key term-raw-map escreen-prefix-char escreen-map)
     (define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
     (define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen)
     (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
     (setq ansi-term-color-vector
	   [unspecified "black" "#f57900" "#8ae234" "#edd400" "#729fcf"
			"#ad7fa8" "cyan3" "#eeeeec"]
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

     (define-key term-raw-map (kbd "C-y")  'term-paste)
     (define-key term-raw-map (kbd "<C-right>")  'cw:term:forward-word)
     (define-key term-raw-map (kbd "<C-left>")  'cw:term:backward-word)
     (define-key term-raw-map (kbd "C-c C-'")  'cw:term:toggle-line-mode)
     (define-key term-mode-map (kbd "C-c C-'")  'cw:term:toggle-line-mode)
     (define-key term-raw-map [mouse-2] 'term-mouse-paste)))

(eval-after-load 'time-stamp
  '(progn
     (setq
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "Last changed:\\\\? "  ; start of pattern
      time-stamp-end "\\\\?\n"                ; end of pattern
      time-stamp-active t                     ; do enable time-stamps
      time-stamp-line-limit 0)
     (make-variable-buffer-local 'time-stamp-start)))

(eval-after-load 'tool-bar
  '(progn (tool-bar-mode -1)))

(eval-after-load 'tramp
  '(progn

     (defadvice tramp-error
       (around cw:tramp-error activate)
       "Allow to use sudo on a remote host:
/sudo:x@y:z ==> /multi:sshx:y:sudo:z@y:z

Based on TWB hack (http://paste.lisp.org/display/90780)."
       ;;(message (format "TRAMP-ERROR(%s %s)" vec-or-proc signal))
       (if (and (eq 'file-error signal)
		(string= "sudo" (tramp-file-name-method vec-or-proc))
		(boundp 'target-alist))
	   (progn
	     ;;(message (format "target-alist: %s" target-alist))
	     (setq target-alist
		   (cons (vector "sshx" ""
				 (tramp-file-name-host vec-or-proc)
				 "")
			 (list (vector (tramp-file-name-method vec-or-proc)
				       (unless (string= "root" (tramp-file-name-user vec-or-proc))
					 (tramp-file-name-user vec-or-proc))
				       (tramp-file-name-host vec-or-proc)
				       (tramp-file-name-localname vec-or-proc))))))
	 ad-do-it))


     (setq
      tramp-default-method "scp"
      tramp-terminal-type "screen"
      tramp-backup-directory-alist backup-directory-alist)))

(eval-after-load 'tramp-sh
  '(progn
     ;; Reload `tramp-compute-multi-hops' to make `cw:tramp-error' advice
     ;; work. WHY ????"
     (find-library "tramp-sh")
     (find-function 'tramp-compute-multi-hops)
     (forward-sexp)
     (eval-last-sexp nil)
     (kill-buffer "tramp-sh.el.gz")

     (defadvice tramp-open-connection-setup-interactive-shell 
       (before cw:tramp-open-connection-setup-interactive-shell activate)
       "Add process-sentinel to tramp-shells. Kill buffer when process died."
       (set-process-sentinel
	;; Arg 0 is proc
	(ad-get-arg 0)
	(lambda (proc change)
	  (when (eq (process-status proc) 'exit)
	    (kill-buffer (process-buffer proc))))))))

 ;; u
(eval-after-load 'uniquify
  '(progn
     (setq uniquify-buffer-name-style 'post-forward)))

 ;; x
(eval-after-load 'x-win
  '(progn
     (setq x-select-enable-primary t)))

 ;; y
(eval-after-load 'yasnippet
  '(progn
     (defvar cw:yasnippet:in-expansionp nil
       "Define if snippet is in expansion phase or not.")
     (make-local-variable 'cw:yasnippet:in-expansionp)

     (defun cw:yasnippet:toggle-expansion-semaphore ()
       "Toggles `cw:yasnippet:in-expansionp'."
       (setq cw:yasnippet:in-expansionp (not cw:yasnippet:in-expansionp)))

     (add-hook 'yas/after-exit-snippet-hook 'cw:yasnippet:toggle-expansion-semaphore)
     (add-hook 'yas/before-expand-snippet-hook 'cw:yasnippet:toggle-expansion-semaphore)

     (setq-default yas/dont-activate 'cw:yasnippet:do-activatep)
     (setq yas/trigger-key "M-TAB"
	   yas/snippet-dirs "~/.emacs.d/templates"
	   yas/prompt-functions '(yas/completing-prompt))
     (yas/load-directory yas/root-directory)
     (add-hook 'yas/minor-mode-hook 'cw:yasnippet:insert-snippet-new-file)))

 ;; w
(eval-after-load 'windmove
  '(progn
     (windmove-default-keybindings)
     (setq windmove-wrap-around t)))

;; Init
(defun cw:init()
  "Initialize all settings"

  ;; emacs C-defined variables
  (setq-default fill-column 76)
  (setq message-log-max 2048)

  ;; Key bindings

  ;; browse-kill-ring
  (define-key global-map (kbd "C-M-y") 'browse-kill-ring)
  ;; flyspell
  (define-key global-map (kbd "M-`") 'ispell-word)
  ;; hippie-exp
  (define-key global-map (kbd "M-/") 'hippie-expand)
  ;; ibuffer
  (define-key global-map (kbd "C-x B") 'ibuffer)
  ;; iedit
  (define-key global-map (kbd "C-;") 'iedit-mode)
  (define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
  ;; magit
  (global-set-key (kbd "C-x C-z") 'magit-status)
  ;; smex
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "ESC M-x") 'execute-extended-command)
  ;; undo-tree
  (global-set-key (kbd "C-x Z") 'undo-tree-visualize)

  (global-set-key (kbd "<C-f1>") 'gnus)

  ;; cw-functions
  (global-set-key (kbd "C-%") 'goto-match-paren)
  (global-set-key (kbd "C-c C-d") 'cw:diff-with-file)
  (global-set-key (kbd "<C-f2>") 'cw:erc:switch-to-screen)
  (define-key global-map (kbd "C-x C-d") 'cw:dired)

  (global-set-key (kbd "C-x <C-return>") 'cw:shell-run)
  (global-set-key (kbd "C-x <S-return>") 'cw:term-run)

  ;; Global activations
  (desktop-save-mode 1)
  (savehist-mode 1)
  ;;(menu-bar-mode -1)


  (require 'uniquify nil t)
  (ido-mode t)
  (show-paren-mode t)
  (add-hook 'after-save-hook 'gac-commit-file t nil)
  (add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . adoc-mode))
  (global-hl-line-mode)
  (when (functionp 'qbs-init) (qbs-init))
  (when (functionp 'cw:gtd:init) (cw:gtd:init))
  (when (functionp 'escreen-install) (escreen-install))

  ;; (when (require 'ace-jump-mode nil t)
  ;;   (global-set-key (kbd "C-x C-PC") 'ace-jump-mode))


  (when (require 'yasnippet nil t)
    (yas/initialize))

  ;; user defined completing-read-function entered in emacs24
  (when (boundp 'completing-read-function)
    (defun ido-completing-read* (prompt choices &optional predicate require-match
				      initial-input hist def inherit-input-method)
      "Adjust arguments when it's necessary"
      (if (and (listp choices) (not (functionp choices)))
	  (ido-completing-read
	   prompt
	   (mapcar (lambda (c) (if (listp c) (car c) c)) choices)
	   predicate require-match initial-input hist def inherit-input-method)
	(completing-read-default prompt choices predicate require-match
				 initial-input hist def inherit-input-method)))
  
    (setq completing-read-function 'ido-completing-read*)))

(unless noninteractive (cw:init))

(provide 'cw-local)
