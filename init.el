;;; init.el --- My Emacs configuration.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2011-05-18 23:54:38
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


;;; Code:


(setq message-log-max 2048)
(setq backup-directory-alist
      `((".*" .  "~/.emacs.d/.tmp/backup")))
(setq auto-save-list-file-prefix "~/.emacs.d/.tmp/auto-save-list/saves-")

(setq ido-save-directory-list-file "~/.emacs.d/.tmp/ido-last")
(setq abbrev-file-name "~/.emacs.d/.tmp/abbrev_defs")
(setq tramp-persistency-file-name "~/.emacs.d/.tmp/tramp")
(setq org-publish-timestamp-directory "~/.emacs.d/.tmp/org-timestamps")

(setq 
 desktop-base-file-name "~/.emacs.d/.tmp/desktop/desktop"
 desktop-base-lock-name "~/.emacs.d/.tmp/desktop/desktop.lock"
 desktop-path '("~/.emacs.d/.tmp/desktop"))

(setq mm-default-directory "~/Download/")


(setq
 el-get-sources
 '((:name el-get
	  :after (lambda()
		   (setq el-get-recipe-path-emacswiki "~/.emacs.d/.tmp/emacswiki")))
   color-theme
   (:name color-theme-tango
	  :after (lambda() (color-theme-tango)
		    (set-face-attribute 'comint-highlight-input nil :italic nil)
		    (set-face-attribute 'font-lock-string-face nil :italic nil)))
   (:name hl-sexp
   	  :after (lambda ()
   		   (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
   		   (set-face-attribute 'hl-sexp-face nil :background "#32383a")))
   offlineimap
   (:name chezwam
	  :type git
	  :url "git@github.com:renard/chezwam-el.git"
	  :features chezwam
	  :after (lambda() (require 'chezwam)))
   (:name chezwam-private-emacs-el
	  :type git
	  :url "git.private.cw:git/chezwam-emacs-el.git"
	  :features chezwam-private
	  :after (lambda() (require 'chezwam-private)))
   (:name ssh-config
	  :url "git@github.com:renard/ssh-config-el.git"
	  :after (lambda()
		   (setq sc:ssh-file "~/.emacs.d/el-get/chezwam-private-emacs-el/hosts.org")))
   (:name escreen
	  :type git
	  :url "git@github.com:renard/escreen-el.git"
	  :after (lambda() (require 'chezwam-escreen)))
   (:name gnus-identities
	  :url "git@github.com:renard/gnus-identities.git")
   (:name magit
	  :features magit
	  :after (lambda ()
		   (setq magit-commit-signoff t)
		   (global-set-key (kbd "C-x C-z") 'magit-status)
		   ;;(set-face-attribute 'magit-item-highlight nil :foreground "#32383a")
	
		   (setq magit-save-some-buffers nil)
		   (set-face-attribute 'magit-diff-add nil :foreground "#8ae234")
		   (set-face-attribute 'magit-diff-hunk-header nil :foreground "#fce94f")
		   (set-face-attribute 'magit-diff-file-header nil :foreground "#ad7fa8")
		   (set-face-attribute 'magit-item-highlight nil :background  "#32383a")
		   (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)))
   (:name buffer-move
	  :after (lambda()
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
   (:name nognus
	  :after (lambda()
		   (setq gnus-startup-file "~/.emacs.d/.tmp/gnus/newsrc")
		   (setq gnus-init-file "~/.emacs.d/el-get/chezwam/gnus-init")))
   (:name string-template
	  :url "git@github.com:renard/string-template-el.git")
   (:name cssh
	  :url "git@github.com:renard/cssh.git"
	  :after (lambda()
		   (define-key dired-mode-map (kbd "C-=") 'cssh-term-remote-open)))
   (:name org-website
	  :type git
	  :url "git@github.com:renard/org-website.git")
   switch-window
   vkill
   google-maps
   (:name browse-kill-ring
	  :after (lambda()
		   (define-key global-map (kbd "C-M-y") 'browse-kill-ring)))
   (:name dired-details
	  :after (lambda()
		   (define-key dired-mode-map "/" 'dired-details-toggle)))
   rainbow-mode
   (:name vcl-mode
	  :type git-svn
	  :url "http://varnish-cache.org/svn/trunk/varnish-tools/emacs")
   list-processes+
   mailq
   asciidoc
   (:name adoc-mode
	  :after (lambda()
		   (add-to-list 'auto-mode-alist '("\\.[1-9]\\.txt$" . adoc-mode))
		   (add-hook 'adoc-mode-hook 'flyspell-mode)
		   (add-hook 'adoc-mode-hook 'flyspell-buffer)
		   (define-key adoc-mode-map (kbd "C-c m") 'cw:adoc-mode:compile)))
   xml-rpc-el
   pastebin
   gnuplot-mode
   php-mode-improved
   (:name org-mode
	  :features (org-contacts org-crypt)
	  :after (lambda()
		   (setq
		    org-contacts-files
		    '("~/.emacs.d/el-get/chezwam-private-emacs-el/contacts.org"))
		   (org-crypt-use-before-save-magic)
		   (define-key org-mode-map (kbd "C-c E") 'cw:org:toggle-encryption)))
   (:name undo-tree
	  :features undo-tree
	  :after (lambda()  (global-set-key (kbd "C-x Z") 'undo-tree-visualize)))

   dirtree
   (:name dired-sync
	  :url "git@github.com:renard/dired-sync.git"
	  :after (lambda() (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync)))
   (:name yasnippet
	  :type git-svn
	  :url "http://yasnippet.googlecode.com/svn/trunk/"
	  :after (lambda () (require 'chezwam-yasnippet)))
   dig
   cisco-router-mode
   lua-mode
   (:name emms
   	  :after (lambda ()
		   (require 'emms-browser)
   		   (emms-standard)
   		   ;; (emms-default-players) ; I want VLC mainly
   		   (setq emms-player-list
			 '(emms-player-mplayer-playlist emms-player-mplayer))
   		   ;; Show the current track each time EMMS
   		   ;; starts to play a track with "NP : "
   		   (add-hook 'emms-player-started-hook 'emms-show)
   		   (setq emms-show-format "EMMS Now Playing: %s")
		   (add-to-list 'emms-info-functions 'emms-info-ogginfo)
		   (add-hook 'dired-load-hook
   			     (define-key dired-mode-map (kbd "E") 'emms-play-dired))
		   (define-key emms-browser-mode-map (kbd "P")
		     '(lambda () (interactive) (switch-to-buffer " *EMMS Playlist*")))
		   (defun cw:emms:browser ()
		     (interactive)
		     (emms-add-directory "~/zic")
		     (emms-browser))))
   (:name db-sql
	  :url "git@github.com:renard/db-sql-el.git")
   rst-mode
   keywiz
   nagios-mode
   (:name smex
	  :after (lambda ()
		   (setq smex-save-file "~/.emacs.d/.tmp/smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
		   (global-set-key (kbd "ESC M-x") 'execute-extended-command)))
   (:name iedit
	  :after (lambda ()
		   (define-key global-map (kbd "C-;") 'iedit-mode)
		   (define-key isearch-mode-map (kbd "C-;") 'iedit-mode)))

))



(defun bootstrap-el-get()
  "Install el-get if not installed."
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   '(lambda(s)
      (replace-string
       "git://github.com/dimitri/el-get.git"
       "git@github.com:renard/el-get.git"
       nil (point-min) (point-max))
      (goto-char (point-max))
      (search-backward "(when (eq 0 status)")
      (forward-sexp)
      (backward-char)
      (insert "(bootstrap-el-get-init)")
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun bootstrap-el-get-init ()
  "Add upstream branch to el-get git repository."
  (let* ((default-directory (concat
			     (file-name-as-directory
			      (el-get-package-directory "el-get"))
			     "el-get"))
	 (git (or
	       (executable-find "git")
	       (error "Unable to find `git'"))))
    (call-process git nil nil t "--no-pager" "remote" "add"
		  "upstream" "git://github.com/dimitri/el-get.git"))
  (mapc (lambda (p) (el-get-save-package-status p "installed"))
   	(el-get-package-name-list))
  (message "el-get installed! You should restart emacs now."))


(defun load-el-get ()
  "Load el-get environment."
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))
  (require 'el-get)
  (setq el-get-eval-after-load nil)

  ;; Setup load-path BEFORE anything else, so cyclic dependencies could be
  ;; properly handeled.
  (mapc (lambda(package)
  	  (message (format "Adding source for package %s" package))
  	  (let* ((source (el-get-package-def package))
		 (el-path (el-get-load-path package)))
  	    (mapc (lambda (path)
  		    (message (format "\t adding %s" path))
  		    (el-get-add-path-to-list package 'load-path path))
  		  (if (stringp el-path) (list el-path) el-path))))
  	(el-get-package-name-list))
  (el-get))

;; Load el-get
(if (not (file-directory-p "~/.emacs.d/el-get/"))
    (bootstrap-el-get)
  (load-el-get))
