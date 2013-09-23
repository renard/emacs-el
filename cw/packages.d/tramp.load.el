;;;###autoload
(defmacro cw:with-parse-directory (sudo &rest body)
  "Execute BODY with a declaration of following variables after
setting `default-directory' to the directory of file file visited
in current buffer.

If SUDO is not nil `method' is set to \"sudo\" and `user' to
\"root\".
"
  `(let* ((sudo ,sudo)
	  (current-buffer-dir
	   ;; get directory name from either dired or buffer file name and
	   ;; fall back to nil
	   (or (ignore-errors (dired-current-directory))
	       (ignore-errors (file-name-directory (buffer-file-name)))
	       default-directory))
	  (file-vector
	   ;; get a tramp usable file URI from directory.
	   (or (ignore-errors (tramp-dissect-file-name current-buffer-dir))
	       (tramp-dissect-file-name (concat "/:" current-buffer-dir) 1)))
	  ;; split file URI into its components
	  (method (if ,sudo "sudo" (tramp-file-name-method file-vector)))
	  (user (if ,sudo "root" (tramp-file-name-user file-vector)))
	  (localname (tramp-file-name-localname file-vector))
	  (host (tramp-file-name-host file-vector))
	  (default-directory
	    ;; If no method is defined then the file is local
	    ;; then don't use tramp.
	    (if method
		(tramp-make-tramp-file-name method user host localname)
	      localname)))
     ,@body))

(defun cw:tramp:dissect-file-name(file &optional user)
  (let ((default-directory file))
    (cw:with-parse-directory
     user
     (message "method:            %s" method)
     (message "user:              %s" user)
     (message "localname:         %s" localname)
     (message "host:              %s" host)
     (message "default-directory: %s" default-directory))))


;; Make sure you have that line in /etc/hosts:
;;   127.0.0.1 HOSTNAME
;;
;; Where HOSTNAME is the result of `system-name'
;;
;; Remote sudo root: /sudo:host:
;; Remote sudo user: /sudo:user@host:
;; Local  sudo root: /sudo::
;; Local  sudo user: /sudo:user@:
;; remote          : /host:
;; remote user     : /ssh:user@host:
;;
;; Unfortunately you cannot connect to a host using a specific user name to
;; a remote host using its IP adress: "/ssh:user@10.1.2.3:" does not work.


(add-to-list 'tramp-default-proxies-alist
 	     '(".*" "\\`.+\\'" "/ssh:%h:"))
(setq
 tramp-default-method "scp"
 tramp-terminal-type "screen"
 tramp-backup-directory-alist backup-directory-alist)
