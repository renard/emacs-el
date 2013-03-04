(defvar cw:gnus-sum-mode-threads-list
  '((".*" (progn
	    (gnus-summary-toggle-threads 1)
	    (gnus-summary-sort-by-date 1))))
  "List defining how to sort messages in Summary buffer")

;;;###autoload
(defun cw:gnus-sum-mode-threads ()
  "sort message in Summary buffer according to `cw:gnus-sum-mode-threads-list'"
  (loop for rule in cw:gnus-sum-mode-threads-list
	when (string-match (car rule) gnus-newsgroup-name)
	do (progn
	     (message "Matching %S" rule)
	     (eval (cadr rule)))))

(if (running-macosxp)
    (progn
      (defun cw:gnus-sum-mode-open-in-mail.app ()
	"Open current message in Apple Mail.app"
	(interactive)
	(let ((id (elt (gnus-summary-article-header) 4)))
	  (when id
	    (shell-command (format "open -a Mail \"message://%s\"" id)))))
      (define-key gnus-summary-mode-map (kbd "<C-return>")
	'cw:gnus-sum-mode-open-in-mail.app))
  (define-key gnus-summary-mode-map (kbd "<C-return>")
    'gnus-article-browse-html-article))

(add-hook 'gnus-summary-prepared-hook 'cw:gnus-sum-mode-threads)

(setq
 gnus-user-date-format-alist
 '(
   ((gnus-seconds-today) .           "Today      %H:%M")
   ((+ 86400 (gnus-seconds-today)) . "%Y-%m-%d %H:%M")
   (604800 .                         "%Y-%m-%d %H:%M") ;;that's one week
   ((gnus-seconds-month) .           "%Y-%m-%d %H:%M")
   ((gnus-seconds-year) .            "%Y-%m-%d %H:%M")
   (t .                              "%Y-%m-%d %H:%M"))
 gnus-summary-line-format (concat "%U%R %5,5k %&user-date; %-20,20n %B%s\n")
 ;;gnus-group-line-format "%M%S%p%P%5uy:%B%(%g%)%O\n"
 ;;gnus-group-line-format "%M%S%p%P:%B%(%g%)%O\n"
 gnus-sum-thread-tree-false-root " ♽ "
 gnus-sum-thread-tree-single-indent "⚙ "
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-root "⚈ "
 gnus-sum-thread-tree-leaf-with-other "├─►"  ; "┣━► "  "▶"
 gnus-sum-thread-tree-single-leaf     "└─►"  ; "┗━► "
 gnus-sum-thread-tree-vertical        "│"   ; "┆" "┋")  "│" "┆"
 )
