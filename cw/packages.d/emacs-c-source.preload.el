(setq 
 auto-save-list-file-prefix (concat cw:tmp-dir "auto-save-list/saves-")
 message-log-max 2048
 use-dialog-box nil)


(setq-default
 fill-column 76
 locale-coding-system 'utf-8-unix
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system nil
 default-process-coding-system '(utf-8-unix . utf-8-unix))
