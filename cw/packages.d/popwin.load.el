(loop for b in '(("*Ido Completions*" :noselect t)
		 ("*Quail Completions* " :noselect t)
		 ("*Anything Occur*" :noselect t)
		 ("*Disabled Command*" :noselect t)
		 "*Backtrace*"
		 "*ack*"
		 ("*Warnings*" :noselect t)
		 "*magit-edit-log*"
		 "*magit-process*"
		 "*Kill Ring*")
      do (add-to-list
	  'popwin:special-display-config
	  (if (listp b) b (list b))))
