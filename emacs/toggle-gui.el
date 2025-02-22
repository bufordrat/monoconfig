(defun mt-turnon-current-direds (turnon)
  (cl-loop
   for buf being the buffers
   do (with-current-buffer buf
	(when (eq major-mode 'dired-mode)
	  (nerd-icons-dired-mode turnon))))
  (if (equal turnon 1)
      (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
    (remove-hook 'dired-mode-hook #'nerd-icons-dired-mode)))

;; (defun mt-turnon-gui (turnon)
;;   (if (equal turnon 1)
;;       (progn (mt-turnon-current-direds 1)
;; 	     (doom-modeline-mode 1))
;;     (mt-turnon-current-direds 0)
;;     (doom-modeline-mode 0)))

;; (defvar gui-mode nil
;;   "Toggle gui-mode.")

;; (defvar gui-mode-map (make-sparse-keymap)
;;   "The keymap for gui-mode")

;; (add-to-list 'minor-mode-alist '(gui-mode " gui"))
;; (add-to-list 'minor-mode-map-alist (cons 'gui-mode gui-mode-map))

;; (defun gui-mode (&optional ARG)
;;   (interactive (list 'toggle))
;;   (setq gui-mode
;; 	(if (eq ARG 'toggle)
;; 	    (not gui-mode)
;; 	  (> ARG 0)))
;;   (if gui-mode
;;       (progn (mt-turnon-gui 1)
;; 	     (message "gui-mode on"))
;;     (mt-turnon-gui -1)
;;     (message "gui-mode off")))

(define-minor-mode gui-mode
  "Mode for non-nw Emacs."
  :init-value t
  (if gui-mode
      (progn (mt-turnon-current-direds 1)
	     (doom-modeline-mode 1))
    (mt-turnon-current-direds -1)
    (doom-modeline-mode -1)))

;; (if gui-mode
;;       (mt-turnon-gui +1)
;;     (mt-turnon-gui -1))
