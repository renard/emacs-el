(when (boundp 'completing-read-function)
  (defun ido-completing-read* (prompt choices &optional predicate require-match
				      initial-input hist def inherit-input-method)
    "Adjust arguments when it's necessary"
    (if (and (listp choices) (not (functionp choices)))
	(ido-completing-read
	 prompt
	 (mapcar (lambda (c) (if (listp c) (car c) c)) choices)
	 predicate require-match initial-input hist def inherit-input-method)
      (completing-read-default prompt choices predicate require-match
			       initial-input hist def inherit-input-method)))
  (setq
   completing-read-function 'ido-completing-read*
   read-file-name-function 'read-file-name-default))
