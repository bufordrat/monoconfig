(defun mt-debug-print (&rest _)
  (require 'time-stamp)
  (when (called-interactively-p 'interactive)
    (message "Matt invoked kill-emacs"))
  (let ((ts (time-stamp-string))
	(messages-string (with-current-buffer "*Messages*"
			   (buffer-string))))
    (with-temp-file "~/tmp/emacs-log"
      (insert (format "emacs died: %s\n\n%s\n"
		      ts
		      messages-string)))))

(advice-add 'kill-emacs :before #'mt-debug-print)
