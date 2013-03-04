(setq ispell-program-name
      (or
       (locate-file "ispell"   exec-path exec-suffixes 'file-executable-p)
       (locate-file "aspell"   exec-path exec-suffixes 'file-executable-p)))
