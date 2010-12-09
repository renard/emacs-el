;;; init.el --- My Emacs configuration.

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2010-12-09 11:08:33
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(setq backup-directory-alist
      `((".*" .  "~/.emacs.d/.backup")))
(setq auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/.saves-")

;; el-get packages definition
(setq
 el-get-sources
 '(el-get
   color-theme
   color-theme-tango
   bbdb))


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
  (el-get))

;; Load el-get
(if (not (file-directory-p "~/.emacs.d/el-get/"))
    (bootstrap-el-get)
  (load-el-get))
