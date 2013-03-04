(add-to-list 'mailcap-mime-extensions '(".mkv" . "video/x-matroska"))
(add-to-list 'mailcap-mime-extensions '(".mp4" . "video/mp4"))
(add-to-list 'mailcap-mime-extensions '(".f4v" . "video/x-f4v"))
(add-to-list 'mailcap-mime-extensions '(".wmv" . "video/x-wmv"))

(mailcap-parse-mailcaps nil t)
