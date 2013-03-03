(unless noninteractive
  (when window-system
    ;; use fc-list
    (set-face-font
     'default
     (if (running-macosxp)
	 ;;"Monaco-14"
	 "DejaVu Sans Mono-14"
       "DejaVu Sans Mono-10"))))
(set-fontset-font t 'symbol (font-spec :family "FreeSerif"))
(set-face-attribute 'nobreak-space nil :foreground "#fce94f")
(set-face-attribute 'highlight nil :background "#2f3537")
