(defun mt-shell (name)
  (interactive "sShell buffer name: ")
  (shell (format "%s" name)))
