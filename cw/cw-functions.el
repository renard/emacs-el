;;; cw-functions.el --- emacs local functions

;; Copyright © 2010 Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sebastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2011-07-21 19:06:54
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;;    Thesaurus of small useful functions that are not worth for a whole
;;    project.


(require 'cl)

;; 
;; adoc-mode
;;
;;;###autoload
(defvar cw:adoc-mode:makefile
  "~/.emacs.d/Makefiles/asciidoc.mak"
  "Where to find the asciidoc.mak Makefile")

;;;###autoload
(defun cw:new-empty-filep ()
  "Return true if `current-buffer' is opening a new empty file."
  (and (buffer-file-name)
       (not (file-exists-p (buffer-file-name)))
       (= (point-max) 1)))

;;;###autoload
(defun cw:adoc-mode:compile ()
    "Compile manpage and display it in another buffer."
    (interactive)
    (unless (string-match "\\.[1-9]\\.txt$" (buffer-file-name))
      (error "%s does not look like an asciidoc manpage source." (buffer-file-name)))
    (save-buffer)
    (let* ((default-directory (file-name-directory (buffer-file-name)))
           (man (file-name-nondirectory
                 (file-name-sans-extension (buffer-file-name))))
           (target (concat (file-name-directory default-directory) man))
           (process
            (start-process (format "*asciidoc to man %s*" target)
                           target
                           "make" "-k" "-f" cw:adoc-mode:makefile man)))
      (process-put process :target target)
      (set-process-sentinel process 'cw:adoc-mode:sentinel)))

(defun cw:adoc-mode:sentinel (proc change)
  "a simple process sentinel so that we don't display the man page early"
  (when (eq (process-status proc) 'exit)
    (cw:adoc-mode:display-man-page (process-get proc :target))))

(eval-when-compile (require 'man))
(defun cw:adoc-mode:display-man-page (target)
  "Show compiled man page"
  (when (get-buffer (concat "*Man " target "*"))
    (kill-buffer (concat "*Man " target "*")))
  (Man-getpage-in-background target))


;;;###autoload
(defun cw:diff-with-file()
  "Diff current buffer with its associated file."
  (interactive)
  (with-current-buffer (get-buffer (current-buffer))
    (if (and buffer-file-name
             (file-exists-p buffer-file-name))
        (let ((tempfile (make-temp-file "buffer-content-")))
          (unwind-protect
              (progn
                (write-region nil nil tempfile nil 'nomessage)
                (diff buffer-file-name tempfile "-Nu" t)
                (sit-for 0))
            (when (file-exists-p tempfile)
              (delete-file tempfile))))
      (message "Buffer %s has no associated file on disc" (buffer-name))
      ;; Display that message for 1 second so that user can read it
      ;; in the minibuffer.
      (sit-for 1)))
  ;; return always nil, so that save-buffers-kill-emacs will not move
  ;; over to the next unsaved buffer when calling `d'.
  nil)

;;;###autoload
(defun cw:make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
	 (save-restriction
	   (widen)
	   (goto-char (point-min))
	   (save-match-data
	     (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
	      (add-mode (logand ?\111 (default-file-modes))))
	 (or (/= (logand ?\111 current-mode) 0)
	     (zerop add-mode)
	     (set-file-modes (buffer-file-name)
			     (logior current-mode add-mode))))))


;; http://www.emacswiki.org/emacs/ParenthesisMatching
;;;###autoload
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; Yasnippet special

;;;###autoload
(defvar cw:yasnippet:new-file-template "new-file-tpl"
  "Yasnippet template to use when creating a new file using
`cw:yasnippet:insert-snippet-new-file'")

;;;###autoload
(defun cw:yasnippet:insert-snippet-new-file ()
  "Insert a default file skeleton when a new file is created as defined
by `cw:yasnippet:new-file-template'.

To make this tip work, a \"NEWFILE-TEMPLATE.yasnippet\" file
should contain the default file skeleton."
  (interactive)
  (when (cw:new-empty-filep)
    (insert cw:yasnippet:new-file-template)
    (yas/expand)))

;;;###autoload
(defun cw:yasnippet:do-activatep()
  "Workarround for yasnippet to act as a skeleton creator.

For this to work, simply declare:

  (setq-default yas/dont-activate 'cw:yasnippet:do-activatep)
"
  (and yas/snippet-dirs
       (null (yas/get-snippet-tables))))

;; ERC functions
(eval-when-compile (require 'erc nil t))
;;;###autoload
(defun cw:erc:connect ()
  "Connect to erc servers defined in `cw:erc:servers'."
  (interactive)
  (cw:erc:disconnect)
  (mapcar '(lambda(x)
	     (unless (get-buffer (format "*irc: %s*" (car x)))
	       (let ((erc-server-connect-function 'erc-open-tls-stream))
		 (apply 'erc (cdr x))
		 ;; (let ((major-mode 'fundamental-mode))
		 (rename-buffer (format "*irc: %s*" (car x))))))
	  cw:erc:servers))

(defun cw:erc:disconnect()
  "Disonnect from erc servers defined in `cw:erc:servers' and
kill related buffers."
  (interactive)
  (mapc '(lambda(x)
	     (let ((buffer (format "*irc: %s*" (car x))))
	       (when (get-buffer buffer)
		 (kill-buffer buffer))))
	  cw:erc:servers)
  (mapcar '(lambda(x)
	     (set-buffer x)
	     (when (eq major-mode 'erc-mode)
	       (kill-buffer)))
	  (buffer-list)))

(defun cw:erc:list-sorted-buffers ()
  "Sort ERC channel buffer by name."
  (sort (erc-buffer-list nil)
	'(lambda(x y) (string< (buffer-name x) (buffer-name y)))))

(defun cw:erc:channel-next-modified()
  "Switch to next modified channel"
  (interactive)
  (when erc-modified-channels-alist
      (switch-to-buffer (caar erc-modified-channels-alist))))

(defun cw:erc:channel-next()
  "Swith to next ERC buffer."
  (interactive)
  (let* ((blist (cw:erc:list-sorted-buffers))
	(idx (1+ (position (current-buffer) blist))))
    (when (= idx (length blist))
      (setq idx 0))
    (switch-to-buffer (elt blist idx))))

(defun cw:erc:channel-prev()
  "Swith to next ERC buffer."
  (interactive)
  (let* ((blist (cw:erc:list-sorted-buffers))
	(idx (1- (position (current-buffer) blist))))
    (when (< idx 0)
      (setq idx (1+ (length blist))))
    (switch-to-buffer (elt blist idx))))

;;;###autoload
(defun cw:erc:switch-to-screen ()
  "Scwitch to last 2 erc buffer with activity."
  (interactive)
  (unless (member 1 (escreen-get-active-screen-numbers))
    (escreen-create-screen))
  (escreen-goto-screen 1)
  (delete-other-windows)
  (split-window-horizontally)
  (with-current-buffer (or (car (erc-buffer-list)) (car (buffer-list)))
    (unless erc-server-process
      (cw:erc:connect)))
  (let* ((bl (erc-buffer-list))
	 (b1 (car bl))
	 (b2 (cadr bl)))
    (switch-window)
    (switch-to-buffer b1)
    (switch-window)
    (switch-to-buffer b2)))

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
	       "~"))
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

(eval-when-compile (require 'term))
;;;###autoload
(defun cw:term-run (&optional sudo)
  "Run terminal in current buffer directory."
  (interactive "P")
  (cw:with-parse-directory
   sudo
   (progn
     (unless (boundp 'explicit-shell-file-name) (require 'term nil t))
     (let ((shell (or explicit-shell-file-name
		      (getenv "ESHELL")
		      (getenv "SHELL")
		      "/bin/sh"))
	   (sh-cmd)
	   (cmd ""))
       ;; Create shell argument to connect to remote host.
       (when host (setq sh-cmd (concat sh-cmd (format "ssh %s -t" host))))
       (when user (setq sh-cmd (concat sh-cmd (format " sudo -u %s -s" user))))
       (when sh-cmd (setq sh-cmd `("-c" ,sh-cmd)))

       ;; Prepare commands to change directory
       (when localname (setq cmd (format "%s cd %s;" cmd localname)))
       (when host
	 (setq cmd (concat cmd " "
			   ;; enable remote directory tracking
			   "function prompt_cmd { "
			   "echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)};"
			   "echo -e \"\\033AnSiTc\" $(pwd);"
			   "echo -e \"\\033AnSiTh\" ${TRAMP_HOSTNAME-" host "};"
			   " };"
			   "export PROMPT_COMMAND=prompt_cmd;")))
       (setq cmd (concat cmd " "
			 ;; don't store these lines into shell history
			 "history -d $((HISTCMD - 1));\n"))
       ;; Start shell
       (switch-to-buffer 
	(apply
	 'make-term (if host
			(concat "ssh " host)
		      (file-name-nondirectory shell))
	 shell nil sh-cmd))
       (cw:set-process-sentinel-kill-buffer-on-process-exit)
       (term-mode)
       (term-char-mode)
       (process-send-string (get-buffer-process (current-buffer)) cmd)))))

;;;###autoload
(defun cw:set-process-sentinel-kill-buffer-on-process-exit ()
  "Add a sentinel to `current-buffer' to close current buffer
when process state changes to `exit'."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel
     (get-buffer-process (current-buffer))
     (lambda (proc change)
       (when (eq (process-status proc) 'exit)
         (kill-buffer (process-buffer proc)))))))

;;;###autoload
(defun cw:shell:set-font()
  "Set default font for terminal."
  (set
   (make-local-variable 'buffer-face-mode-face)
   '(:family "Terminus" :height 120))
  (buffer-face-mode))

;;;###autoload
(defun cw:shell-run (&optional sudo)
  "Run terminal in current buffer directory."
  (interactive "P")
  (cw:with-parse-directory sudo (progn (shell))))


;; Org functions

;;;###autoload
(defun cw:org:toggle-encryption()
  "Toggle encryption in for current entry."
  (interactive)
  (org-crypt-use-before-save-magic)
  (save-excursion
    (org-back-to-heading t)
    (next-line)
    (if (looking-at "-----BEGIN PGP MESSAGE-----")
	(org-decrypt-entry)
      (org-encrypt-entry))))

;;;###autoload
(defun cw:dired ()
  "Open `default-directory' in `dired' without confirmation."
  (interactive)
  (cw:with-parse-directory (progn (dired default-directory))))