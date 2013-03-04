(add-to-list 'tramp-default-proxies-alist
	     '(".*" "\\`.+\\'" "/ssh:%h:"))
(setq
 tramp-default-method "scp"
 tramp-terminal-type "screen"
 tramp-backup-directory-alist backup-directory-alist)
