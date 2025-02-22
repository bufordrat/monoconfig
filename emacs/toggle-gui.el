(defun mt-turnon-current-direds (turnon)
  (cl-loop
   for buf being the buffers
   do (with-current-buffer buf
	(when (eq major-mode 'dired-mode)
	  (nerd-icons-dired-mode turnon))))
  (if (equal turnon 1)
      (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
    (remove-hook 'dired-mode-hook #'nerd-icons-dired-mode)))

(define-minor-mode gui-mode
  "Mode for non-nw Emacs."
  :init-value t
  (if gui-mode
      (progn (mt-turnon-current-direds 1)
	     (doom-modeline-mode 1))
    (mt-turnon-current-direds -1)
    (doom-modeline-mode -1)))
