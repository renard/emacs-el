(setq
 time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
 time-stamp-start "Last changed:\\\\? "  ; start of pattern
 time-stamp-end "\\\\?\n"                ; end of pattern
 time-stamp-active t                     ; do enable time-stamps
 time-stamp-line-limit 0)
(make-variable-buffer-local 'time-stamp-start)
