;;; init.el --- My Emacs configuration.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2010-12-14 15:37:13
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(setq backup-directory-alist
      `((".*" .  "~/.emacs.d/.tmp/.backup")))
(setq auto-save-list-file-prefix "~/.emacs.d/.tmp/.auto-save-list/.saves-")
;; (setq debug-on-error t)
;; el-get packages definition
(setq
 el-get-sources
 '(el-get
   color-theme
   color-theme-tango

   (:name chezwam
	  :type git
	  :url "git@github.com:renard/chezwam-el.git"
	  :features chezwam)
   (:name chezwam-private-emacs-el
	  :type git
	  :url "git.private.cw:git/chezwam-emacs-el.git"
	  :after (lambda() (require 'chezwam-erc-conf)))
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
   (:name bbdb
	  :after (lambda() (require 'chezwam-bbdb)))
   (:name magit
	  :features magit
	  :after (lambda ()
		   (setq magit-commit-signoff t)
		   (global-set-key (kbd "C-x C-z") 'magit-status)
		   (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)))
   (:name buffer-move
	  :after (lambda()
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))
   (:name nognus
	  :after (lambda()
		   (setq gnus-init-file "~/.emacs.d/el-get/chezwam/gnus-init")))
   (:name string-template
	  :url "git@github.com:renard/string-template-el.git")
   cssh
   switch-window
   vkill
   google-maps
   minimap
   browse-kill-ring
   (:name dired-details
	  :after (lambda()
		   (define-key dired-mode-map "/" 'dired-details-toggle)))
   (:name hl-sexp
	  :after (lambda ()
		   (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)
		   (set-face-attribute 'hl-sexp-face nil :background "#32383a")))
   rainbow-mode
   (:name vcl-mode
	  :type git-svn
	  :url "http://varnish-cache.org/svn/trunk/varnish-tools/emacs")
   list-processes+
   mailq
   (:name auto-complete
	  :after (lambda() (require 'chezwam-auto-complete)))
   asciidoc
   xml-rpc-el
   pastebin
   php-mode-improved
   org-mode
   undo-tree
   dirtree
   (:name dired-sync
	  :url "git@github.com:renard/dired-sync.git")
   (:name yasnippet
	  :type git-svn
	  :url "http://yasnippet.googlecode.com/svn/trunk/"
	  :after (lambda () (require 'chezwam-yasnippet)))
   skype
   dig
   cisco-router-mode
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

  ;; Setup load-path BEFORE anything else, so cyclic dependencies could be
  ;; properly handeled.
  (mapc (lambda(package)
  	  (message (format "Adding source for package %s" package))
  	  (let* ((source (el-get-package-def package))
		 (el-path (or (plist-get source :load-path) '("."))))
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
