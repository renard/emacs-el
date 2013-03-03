(setq epg-gpg-program (or (executable-find "gpg2")
			  (executable-find "gpg")
			  nil))
