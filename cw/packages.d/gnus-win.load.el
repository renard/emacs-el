(gnus-add-configuration
 '(summary
   (vertical 1.0
	     (horizontal 1.0
			 (group 50)
			 (summary 1.0 point)))))

(gnus-add-configuration
 '(article
   (horizontal 8
	       (group 50)
	       (vertical 1.0
			 (summary 20 point)
			 (article 1.0)))))

(loop for type in '(reply reply-yank forward message post mail-bound)
      do (gnus-add-configuration
	  `(,type
	    (horizontal 8
			(group 50)
			(vertical 1.0
				  (summary 20)
				  (,type 1.0 point))))))
