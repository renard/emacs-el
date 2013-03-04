(eval-when-compile (require 'cl))

(defvar cw:el-get-repository "git@github.com:renard/el-get.git"
  "Source from where to fetch `el-get'. If nil, use the official
  el-get repository.")

(defvar cw:home-dir 
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) "cw")))

(defvar cw:packages-config-dir
  (file-name-as-directory
   (concat cw:home-dir "packages.d")))

(defvar cw:private-home-dir
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) "cw-private")))

(defvar cw:private-packages-config-dir
  (file-name-as-directory
   (concat cw:private-home-dir "packages.d")))

(defvar cw:tmp-dir
  (file-name-as-directory
   (concat (file-name-as-directory user-emacs-directory) ".tmp")))

;; Macro definition
(defun running-macosxp ()
  "Return T if running under Mac OS X."
  (string-match "apple-darwin" system-configuration))

(defmacro when-running-macosx (&rest body)
  "eval body only when running under MacOSX"
  `(when (running-macosxp) ,@body))

(loop for dir in (list cw:packages-config-dir cw:tmp-dir)
      unless (file-directory-p dir)
      do (mkdir dir t))

(loop for file in
      (nconc (when (file-directory-p cw:packages-config-dir)
	       (directory-files cw:packages-config-dir t))
	     (when (file-directory-p cw:private-packages-config-dir)
	       (directory-files cw:private-packages-config-dir t)))
      when (and (string-match (format "^.*/\\([^/]+\\)\\.preload\\.el$") file)
		(file-exists-p file))
      do (load file)
      when (and
	    (string-match (format "^.*/\\([^/]+\\)\\.load\\.el$") file)
	    (file-exists-p file))
      do (progn
	   (eval-after-load (match-string-no-properties 1 file)
	     `(load ,file))))

(when-running-macosx
 (loop for d in '("/usr/local/bin" "/usr/texbin")
       do (progn
            (setenv "PATH" (concat d ":" (getenv "PATH")))
            (add-to-list 'exec-path d))))

;; Add some definitions
(loop for p in '("cw-private" "cw" "el-get/el-get")
      do (add-to-list
	  'load-path
	  (concat (file-name-as-directory user-emacs-directory) p)))

;; Load el-get as defined on el-get home page.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let ((el-get-install-skip-emacswiki-recipes t)
	   el-get-master-branch)
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

(let* ((time (time-to-seconds
              (time-subtract (current-time) before-init-time)))
       (msg (format "Emacs loaded in %.3fs" time)))
  (message msg)
  (when (and (not noninteractive)
             (boundp 'el-get-notify-type))
    (el-get-notify "Emacs is ready." msg)))
