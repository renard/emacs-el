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
(defadvice tramp-error
    (around cw:tramp-error activate)
  "Convert \"/scp:user@remote:\" to \"/ssh:user@remote:\" to
  connect to remote host using specific login.

This is compatible with `tramp-default-proxies-alist' with

  '((\".*\" \"\\\\`.+\\\\'\" \"/ssh:%h:\"))

Connection can be done using:

Remote sudo root: /sudo:host:
Remote sudo user: /sudo:user@host:
Local  sudo root: /sudo::
Local  sudo user: /sudo:user@:
remote          : /host:
remote user     : /user@host:"
   (let ((url (cadr target-alist)))
     (if (eq 'file-error signal)
	 (if (string= "scp" (tramp-file-name-method vec-or-proc))
	     (setq target-alist
		   (list (vector
			  "ssh"
			  (tramp-file-name-user vec-or-proc)
			  (tramp-file-name-host vec-or-proc)
			  (tramp-file-name-localname vec-or-proc)
			  nil))))
       ad-do-it)))


;; Local  sudo root: /sudo::
;; Local  sudo user: /sudo:user@:
;; remote          : /host:
;; remote user     : /ssh:user@host:

;; (defadvice tramp-error
;;     (around cw:tramp-error activate)
;;   "Allow to use sudo on a remote host:
;; /sudo:x@y:z ==> /multi:sshx:y:sudo:z@y:z

;; Based on TWB hack (http://paste.lisp.org/display/90780)."
;;   ;;(message (format "TRAMP-ERROR(%s %s)" vec-or-proc signal))
;;   (if (and (eq 'file-error signal)
;; 	   (string= "sudo" (tramp-file-name-method vec-or-proc))
;; 	   (boundp 'target-alist))
;;       (progn
;; 	;;(message (format "target-alist: %s" target-alist))
;; 	(setq target-alist
;; 	      (cons (vector "sshx" ""
;; 			    (tramp-file-name-host vec-or-proc)
;; 			    "")
;; 		    (list (vector (tramp-file-name-method vec-or-proc)
;; 				  (unless (string=
;; 					   "root"
;; 					   (tramp-file-name-user vec-or-proc))
;; 				    (tramp-file-name-user vec-or-proc))
;; 				  (tramp-file-name-host vec-or-proc)
;; 				  (tramp-file-name-localname vec-or-proc))))))
;;     ad-do-it))
;; (ad-deactivate 'tramp-error)


(setq
 tramp-default-method "scp"
 tramp-terminal-type "screen"
 tramp-backup-directory-alist backup-directory-alist)


(add-to-list 'tramp-methods
	     '("vcsh"
	       (tramp-login-program "vcsh")
	       (tramp-login-args (("enter") ("%h")))
	       (tramp-remote-shell "/bin/sh")
	       (tramp-remote-shell-args ("-c"))))
