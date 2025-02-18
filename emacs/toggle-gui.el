(defun mt-turnon-current-direds (turnon)
  (progn (cl-loop
	  for buf being the buffers
	  do (with-current-buffer buf
	       (when (eq major-mode 'dired-mode)
		 (nerd-icons-dired-mode turnon))))
	 (if (equal turnon 1)
	     (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
	   (remove-hook 'dired-mode-hook #'nerd-icons-dired-mode))))

(defun mt-turnon-gui (turnon)
  (if (equal turnon 1)
      (progn (mt-turnon-current-direds 1)
	     (doom-modeline-mode 1))
    (progn (mt-turnon-current-direds 0)
	   (doom-modeline-mode 0))))

