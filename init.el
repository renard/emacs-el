;;; init.el --- My Emacs configuration.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2011-07-25 09:58:58
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


;;; Code:

(eval-when-compile (require 'cl))

;; Set some default directories
(let ((tmp-dir (file-name-as-directory
		(concat (file-name-as-directory user-emacs-directory)
			".tmp"))))

  (mkdir tmp-dir t)
  (setq auto-save-list-file-prefix (concat tmp-dir "auto-save-list/saves-"))

  (eval-after-load 'abbrev
    `(progn
       (setq abbrev-file-name (concat ,tmp-dir "abbrev_defs"))))

  (eval-after-load 'desktop
    `(progn
       (setq
	desktop-base-file-name (concat ,tmp-dir "desktop")
	desktop-base-lock-name (concat ,tmp-dir "desktop.lock")
	desktop-path (list ,tmp-dir))))

  (eval-after-load 'emms
    `(progn 
       (setq emms-directory (concat ,tmp-dir "emms"))
       (mkdir emms-directory t)))

  (eval-after-load 'files
    `(progn
       (setq
	backup-directory-alist `((".*" . ,(concat ,tmp-dir "backup"))))))

  (eval-after-load 'ido
    `(progn
       (setq ido-save-directory-list-file (concat ,tmp-dir "ido-last"))))

  (eval-after-load 'mm-decode
    `(progn
       (setq mm-default-directory "~/Download/")
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

(eval-after-load "el-get"
  '(progn
     ;; define el-get sources
     (setq
      el-get-sources
      '(
	(:name dired-toggle-sudo
	       :type git
	       :url "git@github.com:renard/dired-toggle-sudo.git")
	(:name git-auto-commit
	       :type git
	       :url "git@github.com:renard/git-auto-commit.git")
	(:name quick-buffer-switch
	       :type git
	       :url "git@github.com:renard/quick-buffer-switch.git")
	(:name bzr
	       :type apt-get)
	(:name string-template
	       :url "git@github.com:renard/string-template-el.git")
	(:name org-website
	       :depends string-template
	       :url "git@github.com:renard/org-website.git")
	(:name vcl-mode ;; git-svn is better than just svn
	       :type git-svn
	       :url "http://varnish-cache.org/svn/trunk/varnish-tools/emacs")
	(:name undo-tree
	       :after (lambda()
			(autoload 'undo-tree-visualize "undo-tree")))
	(:name dired-sync
	       :url "git@github.com:renard/dired-sync.git")
	(:name db-sql
	       :url "git@github.com:renard/db-sql-el.git")
	(:name lua-mode
	       :url "https://github.com/immerrr/lua-mode.git")
	(:name ssh-config
	       :url "git@github.com:renard/ssh-config-el.git")
	(:name gnus-identities
	       :url "git@github.com:renard/gnus-identities.git")
	(:name emms ;; Original recipe is buggy
	       :depends emacs-w3m
	       :features nil
	       :build ("mkdir -p ~/.emacs.d/emms"
		       "make autoloads"
		       "make SITEFLAG='--no-site-file -L ~/.emacs.d/el-get/emacs-w3m'"
		       "rm -rf ~/.emacs.d/emms"))
	(:name yasnippet ;; git-svn is better than just svn
	       :url "http://yasnippet.googlecode.com/svn/trunk/"
	       :type git-svn)
	(:name xml-rpc-el
	       :debpends bzr)
	(:name cw-gtd
	       :type git
	       :url  "/home/renard/dev/.emacs.d/cw-gtd")
	(:name mediawiki
	       :after (lambda ()
			(autoload 'mediawiki-open "mediawiki.el")
			(autoload 'mediawiki-site "mediawiki.el")))
	(:name ace-jump
	       :type git
	       :url "https://github.com/winterTTr/ace-jump-mode.git")
	))

     ;; create a package list to be installed
     (let ((cw:packages 
	    '(
	      adoc-mode
	      browse-kill-ring
	      buffer-move
	      cisco-router-mode
	      color-theme
	      color-theme-tango
	      crontab-mode
	      dig
	      dired-details
	      dirtree
	      escreen
	      google-maps
	      iedit
	      keywiz
	      list-processes+
	      magit
	      mailq
	      muse
	      nagios-mode
	      nognus
	      offlineimap
	      org-mode
	      pastebin
	      php-mode-improved
	      rainbow-delimiters
	      rainbow-mode
	      rst-mode
	      smex
	      switch-window
	      vkill

	      )))

       (setq cw:packages
	     (append cw:packages
		     (loop for src in el-get-sources
			   collect (el-get-source-name src))))

       (message "running (el-get 'sync '%s)" cw:packages)
       ;; Really do install packages
       (el-get 'sync cw:packages)
       (require 'cw-local nil t)
       (require 'cw-private nil t))))

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
     ;; Add upstream branch to local repository.
     (replace-string
      "http://github.com/dimitri/el-get.git"
      "git@github.com:renard/el-get.git"
      nil (point-min) (point-max))
     (goto-char (point-max))
     (search-backward "(unless (zerop status)")
     (forward-sexp)
     (insert
      "(cd package)"
      "(call-process git nil `(,buf t) t \"--no-pager\""
      "\"remote\" \"add\" \"--fetch\"  \"upstream\""
      "\"git://github.com/dimitri/el-get.git\")")
     (goto-char (point-max))
     (eval-print-last-sexp))))

(unless noninteractive
  (with-current-buffer
      (find-file
       (concat
	(file-name-as-directory user-emacs-directory)
	"cw-private/cw-pass.el.gpg"))
    (eval-buffer)
    (kill-buffer)))

(let* ((time
	(destructuring-bind (hi lo ms) (current-time)
	  (+
	   (- (+ hi lo) (+ (first before-init-time) (second before-init-time)))
	   (/ (- ms (third before-init-time)) (expt 10.0 6)))))
       (msg (format "Emacs loaded in %.3fs" time)))
  (message msg)
  (unless noninteractive
    (el-get-notify "Emacs is ready." msg)))
