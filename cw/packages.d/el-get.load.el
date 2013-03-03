(setq
 el-get-is-lazy t)

(add-to-list 'el-get-recipe-path
	     (concat cw:home-dir "recipes"))

(let* ((packages (with-temp-buffer
		   (insert-file-contents-literally
		    (concat cw:home-dir "packages.el"))
		   (car (read-from-string (buffer-string)))))
       (installed  (mapcar 'el-get-as-symbol
			   (el-get-list-package-names-with-status
			    "installed"  "removed")))
       (p-diff (set-difference installed packages)))

  (message "%s" p-diff)

  ;; (if installed
  ;;     (el-get nil (when p-diff packages))
  ;;   (el-get nil packages))
  (el-get nil packages))
      

(require 'cw-local nil t)
