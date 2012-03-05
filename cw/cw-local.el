;; init.el --- emacs local configuration

;; Copyright © 2011 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2012-02-21 11:58:11
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
  (autoload 'org-crypt-use-before-save-magic "org-crypt")
  (autoload 'descbinds-anything "descbinds-anything")
  (autoload 'dictionary-new-search "dictionary"))

(load "~/.emacs.d/.tmp/autoload")

 ;; a
(eval-after-load 'adoc-mode
  '(progn
     (add-hook 'adoc-mode-hook 'flyspell-mode)
     (add-hook 'adoc-mode-hook 'flyspell-buffer)
     (define-key adoc-mode-map (kbd "C-c m") 'cw:adoc-mode:compile)))

(eval-after-load 'ack
  '(progn
     (setq ack-command "ack-grep --nocolor --nogroup ")))

(eval-after-load 'ansi-color
  '(progn
     (setq ansi-color-names-vector
	   ["black" "#f57900" "#8ae234" "#edd400" "#729fcf"
	    "#ad7fa8" "cyan3" "#eeeeec"]
	   ansi-color-map (ansi-color-make-color-map))))

(eval-after-load 'anything
  '(progn
       (require 'anything-config nil t)))
 ;; b
(eval-after-load 'browse-url
  '(progn
     (setq
      browse-url-generic-program "raise-x-www-browser"
      browse-url-generic-args nil
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
     (setq
      color-theme-load-all-themes nil
      color-theme-libraries nil)
     (color-theme-tango)
     (set-face-attribute 'comint-highlight-input nil :italic nil)
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

(eval-after-load 'dictionary
  '(progn
     ;; (make-local-hook HOOK)
     ;; This function is obsolete since 21.1;
     ;; not necessary any more.
     (defun make-local-hook (name) "backward compat" nil)
     (setq
      dictionary-server "localhost")))

(eval-after-load 'diff
  '(progn
     (setq diff-switches "-Nu")))

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
     ;;(ad-unadvise 'dired-find-file)

     (defadvice dired-up-directory (around cw:dired-up-directory activate)
       "Replace current buffer with parent dir."
       (let* ((orig (current-buffer)))
     	 ad-do-it
     	 (kill-buffer orig)))
     ;;(ad-unadvise 'dired-up-directory)

     (defun cw:dired-find-file-maybe ()
       "run `dired-find-file' or `dired-maybe-insert-subdir'
depending on the context."
       (interactive)
       (let ((current-file (dired-get-filename)))
	 (when (stringp current-file)
	   (if (file-directory-p current-file)
	       (progn
		 ;;(dired-hide-subdir 1)
		 (message (format "Inserting %S" current-file))
		 (dired-maybe-insert-subdir current-file)
		 (revert-buffer))
	     (if (and (boundp 'ad-Orig-dired-find-file)
		      (functionp 'ad-Orig-dired-find-file))
		 ad-Orig-dired-find-file
	       dired-find-file)))))

     (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

     (define-key dired-mode-map (kbd "<return>") 'dired-find-file)
     (define-key dired-mode-map (kbd "<M-return>") 'cw:dired-find-file-maybe)
     (define-key dired-mode-map (kbd "TAB") 'dired-hide-subdir)
     (define-key dired-mode-map (kbd "<C-return>")  'gnus-dired-find-file-mailcap)
     (define-key dired-mode-map (kbd "<backspace>")  'dired-kill-subdir)
     (define-key dired-mode-map "/" 'dired-details-toggle)
     (define-key dired-mode-map "Y"  'dired-do-relsymlink)
     (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)
     (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync)
     (define-key dired-mode-map (kbd "C-c s") 'dired-do-sync-pool)
     (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)))

 ;; e
(eval-after-load 'emms
  '(progn
     (require 'emms-setup nil t)
     (require 'emms-player-mplayer nil t)
     (require 'emms-info-libtag nil t)
     (require 'emms-browser nil t)

     (setq
      emms-show-format "NP: %s"
      emms-player-list '(emms-player-mplayer-playlist emms-player-mplayer))))

(eval-after-load 'emms-info
  '(progn
     (setq
      emms-info-asynchronously t
      emms-info-functions '(emms-info-libtag))))

(eval-after-load 'emms-mode-line
  '(progn
     (setq
      emms-mode-line-format " %s ")))

(eval-after-load 'emms-setup
  '(progn
     (emms-standard)))

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
      erc-track-exclude-server-buffer t
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
     (define-key escreen-map "l" 'escreen-get-active-screen-numbers-with-emphasis)))

 ;; f
(eval-after-load 'faces
  '(progn
     (unless noninteractive
       ;; use fc-list
       (set-face-font 'default "DejaVu Sans Mono-10"))
     (set-face-attribute 'nobreak-space nil :foreground "#fce94f")))

(eval-after-load 'files
  '(progn
     (add-hook 'after-save-hook 'cw:make-buffer-file-executable-if-script-p)
     (add-hook 'before-save-hook 'time-stamp)))

(eval-after-load 'fill-column-indicator
  '(progn
     (setq
      fci-rule-color "#3e4446"
      fci-rule-width 1
      fci-rule-character ?│
      fci-handle-truncate-lines t)))

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
     (define-key gnus-article-mode-map (kbd "<C-return>") 'gnus-article-browse-html-article)
     (setq
      gnus-visible-headers (concat gnus-visible-headers
				   "\\|^User-Agent:\\|^X-Mailer:")
      gnus-article-update-date-headers nil)))

(eval-after-load 'gnus-msg
  '(progn
     (setq
      gnus-gcc-mark-as-read t)
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
     (define-key gnus-summary-mode-map "F" 'cw:gnus-summary-followup-with-original)))

(eval-after-load 'gnus-start
  '(progn
     (setq gnus-init-file
	   (concat (file-name-as-directory user-emacs-directory) "cw/cw-gnus.el"))
     (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)))

(eval-after-load 'gnus-sum
  '(progn
     (define-key gnus-summary-mode-map (kbd "<C-return>") 'gnus-article-browse-html-article)
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

(eval-after-load "hilit-chg"
  '(progn
     ;; This ugly trick is used to prevent from getting red colored changes
     ;; at startup.
     (defadvice hilit-chg-make-list
       (after cw:hilit-chg-make-list activate)
       "Update face attributes."
       (set-face-attribute 'highlight-changes nil :foreground nil :background "#2e4436")
       (set-face-attribute 'highlight-changes-delete nil :foreground nil :background "#3e3446" :underline nil))
     (hilit-chg-make-list)
     (defun cw:hilit-chg-reset ()
       "Remove highlight marks in current buffer."
       (interactive)
       (highlight-changes-remove-highlight (point-min) (point-max)))
     (add-hook 'after-save-hook 'cw:hilit-chg-reset)))

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


(eval-after-load 'hl-tags-mode
  '(progn
     (set-face-attribute 'hl-tags-face nil :background "#2e4436" :foreground nil)))

 ;; i
(eval-after-load 'ibuffer
  '(progn
     (setq
      ibuffer-saved-filter-groups
      '(("default"
	 ("dired" (mode . dired-mode))
	 ("erc" (mode . erc-mode))
	 ("emacs" (or
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Messages\\*$")
		   (mode . completion-list-mode)
		   (name . "\\*ielm\\*")))
	 ("lisp" (mode . emacs-lisp-mode))
	 ("gnus" (or
		  (mode . message-mode)
		  (mode . bbdb-mode)
		  (mode . mail-mode)
		  (mode . gnus-group-mode)
		  (mode . gnus-summary-mode)
		  (mode . gnus-article-mode)
		  (name . "^\\.bbdb$")
		  (name . "^\\.newsrc-dribble")
		  (name . "^\\*nnimap ")
		  (name . "^\\*imap log\\*")))
	 ("org" (mode . org-mode))
	 ("emms" (or
		  (mode . emms)))
	 ("tramp" (or
		    (name . "^\\*tramp")))
	 ("term" (or
		  (mode . term-mode)
		  (mode . shell-mode)))
	 ("magit" (or
		    (name . "^\\*magit"))))))
     (add-hook 'ibuffer-mode-hook
	       (lambda ()
		 (ibuffer-switch-to-saved-filter-groups "default")))
     (defadvice ibuffer (around cw:ibuffer activate)
       "Open ibuffer with cursour pointed to most recent buffer name."
       (let ((recent-buffer-name (buffer-name)))
	 ad-do-it
	 (ibuffer-jump-to-buffer recent-buffer-name)))))

(eval-after-load 'ido
  '(progn
     ;; ido configuration
     (setq
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-show-dot-for-dired t
      ido-use-url-at-point t
      ido-auto-merge-delay-time 5.00
      ido-default-buffer-method 'selected-window)
     ;; Some ido key bindings
     (define-key global-map (kbd "C-x C-b") 'ido-switch-buffer)
     (define-key global-map (kbd "C-x b") 'ido-switch-buffer)
     (defadvice ido-completion-help (around cw:ido-completion-help activate)
       "Do not create \"Ido Completion Help\" buffer.")
     (defun cw:ido-init-keys ()
       "Add some usefull ido bindings."
       (define-key ido-common-completion-map (kbd "?") 'ad-Orig-ido-completion-help))
     (add-hook 'ido-setup-hook 'cw:ido-init-keys)))

(eval-after-load 'ielm
  '(progn
     (define-key inferior-emacs-lisp-mode-map (kbd "C-c C-f") 'find-function-or-variable-at-point)
     (add-hook 'inferior-emacs-lisp-mode-hook 'eldoc-mode)
     (add-hook 'inferior-emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

(eval-after-load 'ispell
  '(progn
     (setq ispell-program-name
	   (or
	    (locate-file "ispell"   exec-path exec-suffixes 'file-executable-p)
	    (locate-file "aspell"   exec-path exec-suffixes 'file-executable-p)))))

 ;; l
(eval-after-load 'lisp-mode
  '(progn
     (defun cw:emacs-lisp-mode-setup ()
       "Setup for `emacs-lisp-mode'."
       (when (or (not (boundp 'cw:yasnippet:in-expansionp))
		 (and (boundp 'cw:yasnippet:in-expansionp)
		      (not cw:yasnippet:in-expansionp)))
	 (unless (boundp 'cw:org:publishing-project)
	   (hs-minor-mode)
	   (flyspell-prog-mode)))
       (rainbow-delimiters-mode 1))
     (define-key emacs-lisp-mode-map (kbd "C-c C-f") 'find-function-or-variable-at-point)
     (add-hook 'emacs-lisp-mode-hook 'cw:emacs-lisp-mode-setup)
     (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
     (add-hook 'lisp-mode-hook 'cw:emacs-lisp-mode-setup)))

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
     (add-to-list 'mailcap-mime-extensions '(".f4v" . "video/x-matroska"))
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

(eval-after-load 'mouse
  '(progn
     (setq mouse-yank-at-point t)))

 ;; o
(eval-after-load 'o-blog
  `(progn
     (setq ob-async-opts '("--eval" "(require (quote color-theme))"
			    "--eval" "(require (quote color-theme-tango))"))
     (defadvice org-publish-blog (around cw:org-publish-blog activate)
       "Do define `cw:org:publishing-project' before publishing."
       (let ((cw:org:publishing-project t))
	 ad-do-it))
     (defun cw:o-blog:start-httpd ()
       "Start httpd server after blog is published."
       (unless noninteractive
	 (let ((httpd-root (format "%s%s" default-directory
				  (ob:blog-publish-dir BLOG))))
	   (httpd-start)
	   (message (format "Starting web at %s" httpd-root))
	   (browse-url (format "http://127.0.0.1:%d" httpd-port)))))
     (add-hook 'o-blog-after-publish-hook 'cw:o-blog:start-httpd)))


(eval-after-load 'org
  '(progn
     (defun cw:org:org-mode-setup ()
       "Setup buffer for `org-mode' files."
       (unless (and
		(not noninteractive)
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
		 (ispell-change-dictionary (match-string 1)))))

	   (goto-char (point-min))
	   (save-match-data
	     (when (search-forward-regexp
		    "^#\\+INPUT_METHOD:[ \t]+\\(.*\\)"
		    nil t)
	       (message (format "Setting input method %s" (match-string 1)))
	       (ignore-errors
		 (set-input-method (match-string 1))))))))
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
       (setq org-crypt-disable-auto-save t))))

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

(eval-after-load 'popwin
  '(progn
     (loop for b in '("*Ido Completions*"
		      "*Quail Completions*"
		      "*Anything Occur*"
		      "*Disabled Command*"
		      "*Backtrace*"
		      "*Warnings*"
		      "*magit-edit-log*"
		      "*magit-process*"
		      "*Kill Ring*")
	   do (add-to-list
	       'popwin:special-display-config
	       `(,b :noselect t)))
     (setq display-buffer-function 'popwin:display-buffer)))

(eval-after-load 'projects
  '(progn
     (setq
      project-rename-all-buffers t
      project-buffer-name-directory-prefix "»"
      project-root-alist
      '(
	("emacs.d" . "~/.emacs.d")
	("fai" . "~/src/fai-config")
	("qbs" . "~/.emacs.d/el-get/quick-buffer-switch/")))))


 ;; q
(eval-after-load 'quail
  '(progn
     (quail-define-package
      "french-cw-postfix" "French" "C<" t
      "French (Français) input method with postfix modifiers based on
 french-alt-postfix."
      nil t nil nil nil nil nil nil nil nil t)
     (quail-define-rules

      ("<<" ["« "])      ("<<<" ["<<"])
      (">>" [" »"])      (">>>" [">>"])
      ("``" ["“"])       ("```" ["``"])
      ("`'" ["”"])       ("`''" ["`'"])

      ("o^" ?°)          ("o^^" ["o^"])
      ("1^" ?¹)          ("1^^" ["1^"])
      ("2^" ?²)          ("2^^" ["2^"])
      ("3^" ?³)          ("3^^" ["3^"])
      ("4^" ?⁴)          ("4^^" ["4^"])
      ("5^" ?⁵)          ("5^^" ["5^"])
      ("6^" ?⁶)          ("6^^" ["6^"])
      ("7^" ?⁷)          ("7^^" ["7^"])
      ("8^" ?⁸)          ("8^^" ["8^"])
      ("9^" ?⁹)          ("9^^" ["9^"])
      ("0^" ?⁰)          ("0^^" ["0^"])

      ;; Magic letters
      ("ae" ?æ)          ("aee"  ["ae"])
      ("oe" ?œ)          ("oee"  ["oe"])
      ("AE" ?Æ)          ("AEE"  ["AE"])
      ("OE" ?Œ)          ("OEE"  ["OE"])

      ("|:" ?¦)          ("|::"  ["|:"])
      ("p/" ?§)          ("p//"  ["p/"])
      ("p|" ?¶)          ("p||"  ["p|"])
      ("..." ?…)         ("...." ["..."])

      (":))" ?☺)         (":)))" [":))"]);; ☻ or ☺?
      (":((" ?☹)         (":(((" [":(("])

      ("=~" ?≈)          ("=~~"  ["=~"])
      ("=/" ?≠)          ("=//"  ["=/"])

      ;; non breaking space
      ("?" [" ?"])       ("??" ["?"])
      ("!" [" !"])       ("!!" ["!"])
      (";" [" ;"])       (";;" [";"])
      (":" [" :"])       ("::" [":"])
      )

     (defun cw:toggle-input-method ()
       "Toggle between `french-cw-postfix' method  and nil"
       (interactive)
       (if (string= current-input-method "french-cw-postfix")
	   (inactivate-input-method)
	 (set-input-method "french-cw-postfix")))

     (global-set-key (kbd "<f9>") 'cw:toggle-input-method)))

(eval-after-load 'quick-buffer-switch
 '(progn
    (defun cw:qbs-add-sudo-cmd ()
      (define-key quick-buffer-switch-map (kbd "C-s") 'dired-toggle-sudo))
    (add-hook 'qbs-post-init-hook 'cw:qbs-add-sudo-cmd)))

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
      send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "127.0.0.1"
      message-from-style 'angles)))

(eval-after-load 'sgml-mode
  '(progn
     (add-hook 'html-mode-hook 'hl-tags-mode)))


(eval-after-load 'smtpmail
  '(progn
     (setq
      sendmail-query-once-function 'smtpmail-send-it)))

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
	  (delete-horizontal-space t)
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

(eval-after-load 'slime
  '(progn
     (setq inferior-lisp-mode "/usr/bin/sbcl")
     (defun cw:slime-setup()
       "Configure slime interface when connected to slime daemon"
       (common-lisp-mode)
       (hs-minor-mode 0))
     (add-hook 'slime-connected-hook 'cw:slime-setup)))

(eval-after-load 'startup
  '(progn
     (setq inhibit-splash-screen t)))

 ;; t
(eval-after-load 'term
  '(progn
     (unless noninteractive
       (define-key term-raw-map escreen-prefix-char escreen-map)
       (define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
       (define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen)
       (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
       (define-key term-raw-map (kbd "M-'") 'ido-switch-buffer)
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
       (define-key term-raw-map [mouse-2] 'term-mouse-paste))))

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

(eval-after-load "tramp-sh"
  '(progn
     ;; Reload `tramp-compute-multi-hops' to make `cw:tramp-error' advice
     ;; work. WHY ????"
     (let ((buffer (find-library "tramp-sh")))
       (find-function 'tramp-compute-multi-hops)
       (forward-sexp)
       (eval-last-sexp nil)
       (kill-buffer buffer))

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
	   yas/prompt-functions '(yas/completing-prompt))
     (setq yas/snippet-dirs "~/.emacs.d/templates")
     (yas/reload-all)
     (add-hook 'yas/minor-mode-hook 'cw:yasnippet:insert-snippet-new-file)))

 ;; v
(eval-after-load 'vc-hooks
  '(progn
     (setq vc-handled-backends nil)))

(eval-after-load 'vline
  '(progn
     (set-face-attribute 'vline nil :background "#32383a")))

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

  (global-set-key (kbd "C-x K") 'delete-frame)

  (global-set-key (kbd "C-x <C-return>") 'cw:shell-run)
  (global-set-key (kbd "C-x <S-return>") 'cw:term-run)

  (global-set-key (kbd "C-h b") 'descbinds-anything)

  (global-set-key (kbd "<C-XF86AudioPlay>") 'emms-smart-browse)
  (global-set-key (kbd "<S-XF86AudioPause>") 'emms-stop)
  (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
  (global-set-key (kbd "<XF86AudioNext>") 'emms-next)
  (global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)


  (define-key global-map (kbd "C-=") 'cw:open-shell)


  (global-set-key (kbd "C-c C-/") 'webjump++)

  (defun cw:dictionary-search ()
    (interactive)
    (dictionary-search
     (or
      (when (region-active-p)
	(buffer-substring-no-properties
	 (mark) (point)))
      (let ((wap (word-at-point)))
	(when wap (substring-no-properties wap))))
     dictionary-default-dictionary))

  (global-set-key (kbd "C-c ?") 'cw:dictionary-search)

  (qbs-init)
  (require 'projects)
  (defun cw:anything-occur ()
    "Restrict buffer to selection if needed so goto line really
works and run `anything-other-buffer'."
    (interactive)
    (save-restriction
      (when (region-active-p)
	(narrow-to-region (region-beginning) (region-end)))
      (anything-other-buffer 'anything-c-source-occur "*Anything Occur*")))
  (global-set-key (kbd "M-s M-s") 'cw:anything-occur)

  ;; Global activations
  (desktop-save-mode 1)
  (savehist-mode 1)
  ;;(menu-bar-mode -1)

  ;; notification-notify
  (require 'notifications nil t)

  ;; slime support
  (require 'slime nil t)

  (ido-mode t)
  (show-paren-mode t)
  (add-hook 'after-save-hook 'gac-commit-file t nil)
  (add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
  (add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
  (global-hl-line-mode)
  (when (functionp 'qbs-init) (qbs-init))
  (when (functionp 'cw:gtd:init) (cw:gtd:init))
  (when (functionp 'escreen-install) (escreen-install))

  ;; (when (require 'ace-jump-mode nil t)
  ;;   (global-set-key (kbd "C-x C-PC") 'ace-jump-mode))

  (when (require 'yasnippet nil t)
    (yas/initialize))

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
