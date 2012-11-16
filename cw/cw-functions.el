;;; cw-functions.el --- emacs local functions

;; Copyright © 2011 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, configuration
;; Created: 2010-12-09
;; Last changed: 2012-09-20 00:28:30
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
  (mapcar #'(lambda(x)
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
  (mapc #'(lambda(x)
	     (let ((buffer (format "*irc: %s*" (car x))))
	       (when (get-buffer buffer)
		 (kill-buffer buffer))))
	  cw:erc:servers)
  (mapcar #'(lambda(x)
	     (set-buffer x)
	     (when (eq major-mode 'erc-mode)
	       (kill-buffer)))
	  (buffer-list)))

(defun cw:erc:list-sorted-buffers ()
  "Sort ERC channel buffer by name."
  (sort (erc-buffer-list nil)
	#'(lambda(x y) (string< (buffer-name x) (buffer-name y)))))

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

  (when
      (or
       (not (erc-buffer-list))
       (let (do-reconnect)
	 (mapcar (lambda(x)
		   (when (string-match "^\\*irc: " (buffer-name x))
		     (unless (get-buffer-process x)
		       (setq do-reconnect t))))
		 (erc-buffer-list))
	 do-reconnect))
    (cw:erc:disconnect)
    (cw:erc:connect))

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

(defun cw:open-shell (&optional path arg)
  "Open shell to PATH. if `current-prefix-arg' \(C-u\) is used open a term instead.
See `cw:term-run' and `cw:shell-run'."
  (interactive
   (list
    (ido-read-file-name "Shell to: " "/" nil t nil )
    current-prefix-arg))
    (with-temp-buffer
      (let ((default-directory (if (file-directory-p path)
				   path
				 (file-name-nondirectory path))))
	(message "Open shell to %S" default-directory)
	(if arg
	    (cw:term-run)
	  (cw:shell-run)))))

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
			 "history -d $((HISTCMD - 1)); clear; \n"))
       ;; Start shell
       (switch-to-buffer
	(apply  'make-term
	 (append
	  (list
	   (if host (concat "ssh " host)
	     (concat (file-name-nondirectory shell) " " default-directory))
	   shell nil)
	  sh-cmd)))

       (set-process-sentinel
	(get-buffer-process (current-buffer))
	(lambda (proc change)
	  (when (eq (process-status proc) 'exit)
	    (kill-buffer (process-buffer proc)))))

       (term-mode)
       (term-char-mode)
       (process-send-string (get-buffer-process (current-buffer)) cmd)))))

;;;###autoload
(defun cw:shell:set-font()
  "Set default font for terminal."
  (set
   (make-local-variable 'buffer-face-mode-face)
   (if (running-macosxp)
       '(:family "Monaco" :height 120)
   '(:family "Terminus" :height 120)))
  (buffer-face-mode))

;;;###autoload
(defun cw:shell-run (&optional sudo dont-reuse)
  "Run terminal in current buffer directory using sudo if SUDO is
not nil.
If DONT-REUSE is not nil try to use an existing shell."
  (interactive "P")
  (cw:with-parse-directory
   sudo
   (let ((buffer (format "*shell %s*" default-directory)))
     (unless dont-reuse
       (setq buffer (generate-new-buffer-name buffer)))
     (shell buffer))))


;; Org functions

;;;###autoload
(defun cw:org:toggle-encryption()
  "Toggle encryption in for current entry."
  (interactive)
  (org-crypt-use-before-save-magic)
  (save-excursion
    (org-back-to-heading t)
    (org-show-entry)
    (next-line)
    (if (looking-at "-----BEGIN PGP MESSAGE-----")
	(org-decrypt-entry)
      (org-encrypt-entry))))

;;;###autoload
(defun cw:dired ()
  "Open `default-directory' in `dired' without confirmation."
  (interactive)
  ;; You need quick-buffer-switch.el
  ;; https://github.com/renard/quick-buffer-switch
  (let ((marker (qbs-find-buffer-visiting-dir default-directory))
	(filename (buffer-file-name)))
    (if marker
	(progn
	  (switch-to-buffer (marker-buffer marker))
	  (goto-char (marker-position marker)))
      (dired default-directory))
    (when filename
      (search-forward-regexp
       (format " %s$" (file-name-nondirectory filename))))))

;;;###autoload
(defun set-hooks-debug (&optional desactivate)
  "Toggle hook name display when run.

If universal argument or DESACTIVATE is provided, debug is set to
off."
  (interactive "P")
  (if desactivate
      (progn
	(ad-unadvise 'run-hooks)
	(ad-unadvise 'run-hook-with-args))
    (progn
      (defadvice run-hooks (before cw:run-hooks activate)
	"Show hook name on run."
	(with-current-buffer "*Messages*"
	  (insert (format "running hooks: %s\n" (ad-get-args 0)))))
      (defadvice run-hook-with-args (before cw:run-hook-with-args activate)
	"Show hook name on run."
	(with-current-buffer "*Messages*"
	  (insert (format "running hooks with args: %s\n" (ad-get-args 0))))))))

;;;###autoload
(defun asciify-string (string)
"Convert STRING to ASCII string.
For example:
“passé” becomes “passe”"
  (loop for c across string
	with ret
	if (member (get-char-code-property c 'general-category)
		   '(Lu Ll))
	collect (downcase
		 (char-to-string
		  (or (car (get-char-code-property c 'decomposition))
		      c)))
	into ret
	else
	collect (char-to-string c) into ret
	finally return (mapconcat 'identity ret "")))

;;;###autoload
(defun find-function-or-variable-at-point ()
  "Locate function or variable at point."
  (interactive)
  (let ((f-o-v (thing-at-point 'symbol)))
    (when f-o-v
      (setq f-o-v (intern (substring-no-properties f-o-v)))
      (cond
       ((fboundp f-o-v)
	(find-function-other-window f-o-v))
       ((boundp f-o-v)
	(find-variable-other-window f-o-v))
       (t
	(message (format "No definition found for %s" f-o-v)))))))

;;;###autoload
(defun phone-number-bounds-of-phone-number-at-point ()
  "Return the start and end points of a phone number at the current
point.
The result is a paired list of character positions for an phone
number located at the current point in the current buffer. An
phone number is any decimal digit 0 through 9 with an optional
starting plus symbol (`+') and with `.', `-' or space in it."
  (save-excursion
    (skip-chars-backward "()0123456789 .+-")
    (if (looking-at "[()+0-9 .]+")
        (let ((start (point)))
          (skip-chars-forward "()0123456789 .+-")
          (cons start (point)))
      nil)))

(put 'phone-number 'bounds-of-thing-at-point
     'phone-number-bounds-of-phone-number-at-point)

;;;###autoload
(defun org-contact-call(&optional contact type properties cmd)
  "Call CONTACT on its TYPE phone.

If PROPERTIES is not specified, phone number is looked in
properties matching HOME_PHONE, HOME_MOBILE, OFFICE_PHONE and
OFFICE_MOBILE.

CMD is a string used to call the contact. By default it is set to:
\"twinkle --cmd 'call %s' --immediate\"."
  (interactive)
  (let* ((properties (or properties
			 '("HOME_PHONE" "HOME_MOBILE" "OFFICE_PHONE" "OFFICE_MOBILE")))
	 (properties-filter (format "%s={.+}" (mapconcat 'identity properties "={.+}|")))
	 (contact (car (org-contacts-filter
			(or contact
			    (org-completing-read
			     "Call: "
			     (org-contacts-filter nil properties-filter) nil t)))))
	 (number
	  (replace-regexp-in-string
	   "(.+)\\|\\s-+\\|-+\\|\\.+" ""
	   (if type
	       (loop for (k . v) in (nth 2 contact)
		     when (equal k type)
		     return v)
	     (cadr
	      (split-string
	       (completing-read
		"Phone number: "
		(loop for (k . v) in (nth 2 contact)
		      when (member k properties)
		      collect (format "%s:%s" k v))
		nil t)
	       ":")))))
	 (cmd (or cmd "twinkle --cmd 'call %s' --immediate"))


	 (cmd-line (list "sh" "-c" (format cmd number)))
	 (cmd-buf (get-buffer-create
		   (format " *Calling: %s*"
			   (substring-no-properties (car contact)))))
	 (proc (apply 'start-process (car cmd-line)
		      cmd-buf (car cmd-line) (cdr cmd-line))))

    (process-put proc :cmd cmd)
    (process-put proc :cmd-buf cmd-buf)

    (set-process-sentinel
     proc
     '(lambda (proc change)
	(when (eq (process-status proc) 'exit)
	  (let ((status  (process-exit-status proc))
		(cmd (process-get proc :cmd))
		(cmd-buf (process-get proc :cmd-buf)))
	    (when (and
		   (not (eq 0 status))
		   (process-buffer proc))
	      (set-window-buffer (selected-window) cmd-buf)
	      (error "Call error: %s" cmd))
	    (when cmd-buf (kill-buffer cmd-buf))))))))

;;;###autoload
(defun save-file-and-copy ()
  "Save current buffer and copy current file to an other file."
  (interactive)
  (save-buffer)
  (let ((filename (read-file-name "Copy file to: " default-directory
				  (expand-file-name
				   (file-name-nondirectory (buffer-name))
				   default-directory)
				  nil nil)))
    (and (file-exists-p filename)
	 (or (y-or-n-p (format "File `%s' exists; overwrite? " filename))
	     (error "Canceled")))
    (write-region (point-min) (point-max) filename)))

(defun cw:time-diff (t1 t2)
  "Compute time delta between T1 and T2."
  (let* ((t1 (if (stringp t1) (date-to-time t1) t1))
	 (t2 (if (stringp t2) (date-to-time t2) t2))
	 t3)
    (when (time-less-p t1 t2)
      (setq t3 t1
	    t1 t2
	    t2 t3))
    (message (format-time-string "%H:%M:%S" (time-subtract t1 t2)))))


;; a little timing facility, useful at the *ielm* prompt e.g.
;; will discard your results -- part of why it's useful at the prompt.
;;;###autoload
(defun %time (fn)
  (let* ((t0 (current-time)))
    (funcall fn)
    (time-to-seconds (time-subtract (current-time) t0))))
;;;###autoload
(defmacro time (form)
  `(%time (lambda () ,form)))
(defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))

;;;###autoload
(defun cw:slime:ccl ()
  "Configure slime for CCL."
  (interactive)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program
	(loop for p in '("/usr/bin/ccl"
			 "~/src/ccl/lx86cl64")
	      until (file-exists-p p)
	      finally return (concat p " -K utf-8"))))
;;;###autoload
(defun cw:build-latex(&optional file rounds)
  "Compile FILE or local buffer or an asked file asynchronously
using `tex-command' ROUNDS times.

If run using `prefix-argument' the document will be processed 3
time."
  (interactive
   (list
    (or
     (when (eq major-mode 'latex-mode) (buffer-file-name))
     (read-file-name "Publish LaTeX from: " nil nil t))
    (if current-prefix-arg 3 1)))

  (let* ((file (expand-file-name file))
	 (default-directory (file-name-directory file))
	 (cmd-line (list
		    tex-command "-interaction" "nonstopmode"
		    "-shell-escape" "-output-directory" "."
		    file))
	 (cmd-buf-name (format "*Building %s*"
			       (file-name-nondirectory file)))
	 (cmd-buf (get-buffer-create cmd-buf-name))
	 (proc (apply 'start-process (car cmd-line) cmd-buf
		      (car cmd-line) (cdr cmd-line)))

	 (proc-sentinel
	  (lambda (proc change)
	    (when (eq (process-status proc) 'exit)
	      (let ((status  (process-exit-status proc))
		    (proc-buf (process-buffer proc))
		    (file (process-get proc :file))
		    (rounds (1- (process-get proc :rounds))))

		(if (not (eq 0 status))
		    (progn
		      (when proc-buf
			(set-window-buffer (selected-window) proc-buf))
		      (error "Failed to build %s" file))

		  (when proc-buf (kill-buffer proc-buf))
		  (if (> rounds 0)
		      (cw:build-latex file rounds)
		    (let ((f (file-name-sans-extension file)))
		      (loop for e in '("aux" "log" "nav" "out" "pyg" "snm" "toc")
			    for fe = (format "%s.%s" f e)
			    when (file-exists-p fe)
			    do (delete-file fe))
		      (shell-command (format "open \"%s.pdf\"" f)))
		    (message  "Built %s" file))))))))

    (process-put proc :file file)
    (process-put proc :rounds rounds)
    (set-process-sentinel proc proc-sentinel)
    (message "Building %s (%d round%s left)"
	     file rounds (if (> rounds 1) "s" ""))))
