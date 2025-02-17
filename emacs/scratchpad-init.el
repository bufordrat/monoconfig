(defun mt-turnon-current-direds (turnon)
  (cl-loop
   for buf being the buffers
   do (with-current-buffer buf
	(when (eq major-mode 'dired-mode)
          (nerd-icons-dired-mode turnon)))))

(defun mt-turnon-gui (turnon)
  (if turnon
    (progn (mt-turnon-current-direds turnon)
	   (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
	   (doom-modeline-mode 1))
    (progn (mt-turnon-current-direds (not turnon))
	     (remove-hook 'dired-mode-hook #'nerd-icons-dired-mode)
	     (doom-modeline-mode 0))))
