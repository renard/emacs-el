;;;###autoload
(defun cw:erc:connect ()
  "Connect to erc servers defined in `cw:erc:servers'."
  (interactive)
  (cw:erc:disconnect)
  (let ((default-directory "~"))
    (mapcar #'(lambda(x)
		(unless (get-buffer (format "*irc: %s*" (car x)))
		  (let ((erc-server-connect-function 'erc-open-tls-stream))
		  (apply 'erc (cdr x))
		  ;; (let ((major-mode 'fundamental-mode))
		  (rename-buffer (format "*irc: %s*" (car x))))))
	    cw:erc:servers)
    (loop for b in (erc-buffer-list)
	  do (with-current-buffer b (erc-cmd-AWAY "")))))

(defun cw:erc:disconnect()
  "Disonnect from erc servers defined in `cw:erc:servers' and
kill related buffers."
  (interactive)
  ;; Remove bogus timers
  ;; See http://lists.gnu.org/archive/html/bug-gnu-emacs/2013-02/msg00053.html
  (loop for timer in timer-list
	when (and
	      (eq 'erc-server-send-ping (timer--function timer))
	      (bufferp (car (timer--args timer)))
	      (not (buffer-name (car (timer--args timer)))))
	do (cancel-timer timer))
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
    (switch-to-buffer b1)
    (select-window (split-window-horizontally))
    (switch-to-buffer b2)))

(unless (featurep 'erc)
  (require 'erc nil t))

(setq
 erc-server-auto-reconnect nil
 erc-kill-server-buffer-on-quit t)
(set-face-attribute 'erc-notice-face nil :foreground "#ad7fa8")
(set-face-attribute 'erc-input-face nil :foreground "#babdb6")
(set-face-attribute 'erc-my-nick-face nil :foreground "#729fcf")

(define-key erc-mode-map (kbd "C-x k") 'erc-iswitchb)
(define-key erc-mode-map (kbd "C-C C-\\") 'cw:erc:channel-next-modified)
(define-key erc-mode-map (kbd "C-C C-]") 'cw:erc:channel-next)
(define-key erc-mode-map (kbd "C-C C-[") 'cw:erc:channel-prev)

(defun cw:erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	(target (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
	       (not (erc-is-message-ctcp-and-not-action-p msg)))
      (if (running-macosxp)
	  (growl (format "ERC: %s" nick) msg)
	(notifications-notify
	 :title nick
	 :body msg
	 :app-icon nil
	 :urgency 'low))))
  ;; Return nil to continue processing by ERC
  nil)

(define-erc-module cw:page-me nil "page me on private message"
  ((add-hook 'erc-server-PRIVMSG-functions 'cw:erc-page-me-PRIVMSG))
  ((remove-hook 'erc-server-PRIVMSG-functions 'cw:erc-page-me-PRIVMSG)))
