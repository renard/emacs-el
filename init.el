;;; init.el --- My Emacs configuration.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2012-09-26 10:56:38
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


;;; Code:


(eval-when-compile (require 'cl))

(defvar cw:el-get-repository "git@github.com:renard/el-get.git"
  "Source from where to fetch `el-get'. If nil, use the official
  el-get repository.")

 ;; Macro definition
(defun running-macosxp ()
  "Return T if running under Mac OS X."
  (string-match "apple-darwin" system-configuration))

(defmacro when-running-macosx (&rest body)
  "eval body only when running under MacOSX"
  `(when (running-macosxp) ,@body))

(when-running-macosx
 (loop for d in '("/usr/local/bin")
       do (progn
            (setenv "PATH" (concat d ":" (getenv "PATH")))
            (add-to-list 'exec-path d))))

;; Set some default directories
(let ((tmp-dir (file-name-as-directory
		(concat (file-name-as-directory user-emacs-directory)
			".tmp"))))

  (mkdir tmp-dir t)
  (setq auto-save-list-file-prefix (concat tmp-dir "auto-save-list/saves-"))

  (eval-after-load 'abbrev
    `(progn
       (setq abbrev-file-name (concat ,tmp-dir "abbrev_defs"))))

  (eval-after-load 'bbdb
    `(progn
       (setq bbdb-file (concat ,tmp-dir "bbdb"))))

  (eval-after-load 'cus-edit
    `(progn
       ;; Do not save custom variables.
       (setq custom-file (concat ,tmp-dir "custom"))))

  (eval-after-load 'desktop
    `(progn
       (setq
	desktop-base-file-name (concat ,tmp-dir "desktop")
	desktop-base-lock-name (concat ,tmp-dir "desktop.lock")
	desktop-path (list ,tmp-dir))))

  (eval-after-load 'el-get
    `(progn
       (setq el-get-recipe-path-emacswiki (concat ,tmp-dir "emacswiki"))))

  (eval-after-load 'emms
    `(progn
       (setq emms-directory (concat ,tmp-dir "emms"))
       (mkdir emms-directory t)))

  (eval-after-load 'eshell
    `(progn
       (setq eshell-directory-name (concat ,tmp-dir "eshell"))))

  (eval-after-load 'files
    `(progn
       (setq
	backup-directory-alist `((".*" . ,(concat ,tmp-dir "backup"))))))

  (eval-after-load 'ido
    `(progn
       (setq ido-save-directory-list-file (concat ,tmp-dir "ido-last"))))

  (eval-after-load 'mm-decode
    `(progn
       (setq mm-default-directory "~/Downloads/gnus")
       (mkdir mm-default-directory t)))

  (eval-after-load 'gnus-start
    `(progn
       (setq gnus-startup-file (concat ,tmp-dir "gnus/newsrc"))
       (mkdir (file-name-directory gnus-startup-file) t)))

  (eval-after-load 'org-clock
    `(progn
       (setq org-clock-persist-file (concat ,tmp-dir "org/org-clock-save"))
       (mkdir (file-name-directory org-clock-persist-file) t)))

  (eval-after-load 'savehist
    `(progn
       (setq savehist-file (concat ,tmp-dir "history"))))

  (eval-after-load 'smex
    `(progn
       (setq smex-save-file (concat ,tmp-dir "smex-items"))))


  (eval-after-load 'tramp-cache
    `(progn
       (setq tramp-persistency-file-name (concat ,tmp-dir "tramp"))))

  (eval-after-load 'url
    `(progn
       (setq url-configuration-directory (concat ,tmp-dir "url")))))

 ;; el-get configuration
(eval-after-load 'el-get-github
  '(progn
     (setq el-get-github-default-url-type 'git)))

(eval-after-load "el-get"
  '(progn
     (add-to-list 'el-get-recipe-path
                  (concat (file-name-as-directory user-emacs-directory)
                          "cw/recipes"))
    ;; define el-get sources
     (unless (running-macosxp)
       (add-to-list
	'el-get-sources
	'(:name emms ;; Original recipe is buggy
		:url "git@github.com:renard/emms.git"
		:depends emacs-w3m
		:description "The Emacs Multimedia System"
		:features nil
		:build `("mkdir -p ~/.emacs.d/emms"
			 "make autoloads"
			 ,(concat "make SITEFLAG='--no-site-file -L ~/.emacs.d/el-get/emacs-w3m' EMACS=" el-get-emacs))
		"rm -rf ~/.emacs.d/emms")))
     ;; create a package list to be installed
     (let ((cw:packages
	    `(
	      ;; nognus should be first item to be loaded otherwise emacs'
	      ;; version of gnus-util is loaded instead.
	      nognus
	      org-mode

	      adoc-mode
	      anything
	      bbdb
	      browse-kill-ring
	      buffer-move
	      cisco-router-mode
	      color-theme
	      color-theme-tango
	      crontab-mode
	      cw-gtd
	      db-sql
	      descbinds-anything
	      dictionary
	      dictionary-app
	      dig
	      dired-details
	      dired-sync
	      dired-toggle-sudo
	      dirtree
	      dpans2texi
	      emacs-http-server
	      escreen
	      expand-region
	      fill-column-indicator
	      filladapt
	      git-auto-commit
	      google-maps
	      gnus-identities
	      iedit
	      indirect-region
	      keywiz
	      list-processes+
	      lua-mode
	      lusty-explorer
	      magit
	      magit-identities
	      mailq
	      maxframe
	      multiple-cursors
	      muse
	      nagios-mode
	      o-blog
	      offlineimap
	      org-website
	      org2blog
	      ;;pastebin
	      php-mode-improved
	      popwin
	      quick-buffer-switch
	      rainbow-delimiters
	      rainbow-mode
	      rst-mode
	      smex
	      srep
	      ssh-config
	      switch-window
	      undo-tree
	      vcl-mode
	      vkill
	      vline
	      volatile-highlights
	      hl-tags-mode
	      webjump++
	      yasnippet
	      xml-rpc-el
	      )))

       (when (running-macosxp)
	 (add-to-list 'cw:packages 'terminal-notifier-el))

       (setq cw:packages
	     (append cw:packages
		     (loop for src in el-get-sources
			   collect (el-get-source-name src))))

       (let ((p-diff (set-difference
		      (mapcar 'el-get-as-symbol
			      (el-get-list-package-names-with-status
			       "installed"  "removed"))
		      cw:packages)))
	 (when p-diff
	   (warn "Following packages are not defined in cw:packages: %s" p-diff)))

       (message "running (el-get 'sync '%s)" cw:packages)
       ;; Really do install packages
       (el-get 'sync cw:packages)
       (require 'cw-local nil t)
       (require 'cw-private nil t)
       (require 'cw-local-fix nil t))))

;; Add some definitions
(loop for p in '("cw" "cw-private" "el-get/el-get")
      do (add-to-list 'load-path
		      (concat (file-name-as-directory user-emacs-directory) p)))

(unless noninteractive
  (require 'server)
  (unless (server-running-p)
    (server-start))
  ;; must be set before Org is loaded.
  (setq org-replace-disputed-keys t))

;; Load el-get
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (when cw:el-get-repository
	 ;; Add upstream branch to local repository.
	 (replace-string
	  "http://github.com/dimitri/el-get.git"
	  cw:el-get-repository
	  nil (point-min) (point-max))
	 (goto-char (point-max))
	 (search-backward "(unless (zerop status)")
	 (forward-sexp)
	 (insert
	  "(cd package)"
	  "(call-process git nil `(,buf t) t \"--no-pager\""
	  "\"remote\" \"add\" \"--fetch\"  \"upstream\""
	  "\"git://github.com/dimitri/el-get.git\")"))
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(unless noninteractive
  (let ((pass-file (concat
	(file-name-as-directory user-emacs-directory)
	"cw-private/cw-pass.el.gpg")))
    (require 'epg-config)
    (when (and
	   (file-readable-p pass-file)
	   (boundp 'epg-gpg-program))
      (with-current-buffer
	  (find-file pass-file)
	(eval-buffer)
	(kill-buffer)))))

(let* ((time (time-to-seconds
	      (time-subtract (current-time) before-init-time)))
       (msg (format "Emacs loaded in %.3fs" time)))
  (message msg)
  (when (and (not noninteractive)
	     (boundp 'el-get-notify-type))
    (el-get-notify "Emacs is ready." msg)))
