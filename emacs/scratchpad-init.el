(defun mt-debug-print ()
  (let ((ts (time-stamp-string))
	(messages-string (with-current-buffer "*Messages*"
			   (buffer-string))))
    (with-temp-file "~/tmp/emacs-log"
      (insert (format "emacs died: %s\n\n%s\n"
		      ts
		      messages-string)))))
