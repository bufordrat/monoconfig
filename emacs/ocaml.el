(defun mli-p (path)
  (and (not (file-directory-p path))
       (or (equal (file-name-extension path) "mli")
	   (equal (file-name-extension path) "shutoffmli"))))

(defun transform-path (old-path)
  (let* ((old-filename (file-name-sans-extension old-path))
	 (old-extension (file-name-extension old-path))
	 (new-extension (cond ((equal old-extension "mli") "shutoffmli")
			      ((equal old-extension "shutoffmli") "mli")
			      (t old-extension)))
	 (new-path (if old-extension
		       (file-name-with-extension old-filename new-extension)
		     old-path)))
    new-path))

(defun mli-dired-toggle ()
  (interactive)
  (let* ((old-path (dired-get-filename))
	 (new-path (transform-path old-path)))
    (unless (mli-p old-path)
      (error "Not an .mli file."))
    (rename-file old-path new-path)
    (revert-buffer)))

(defun suitable-mli-buffer-p ()
  (if buffer-file-name
      (let ((current-extension (file-name-extension buffer-file-name)))
	(or (eq major-mode 'tuareg-mode)
	    (equal current-extension "shutoffmli")))
    nil))

(defun suitable-ml-buffer-p ()
  (if buffer-file-name
      (let ((current-extension (file-name-extension buffer-file-name)))
	(and (eq major-mode 'tuareg-mode)
	     (equal current-extension "ml")))
    nil))

(defun mli-buffer-toggle ()
  (let* ((old-path buffer-file-name)
	 (new-path (transform-path old-path)))
    (rename-file old-path new-path)
    (find-alternate-file new-path)
    (revert-buffer nil t)))

(defun derive-mli-path (old-path)
  (let* ((old-filename (file-name-sans-extension old-path))
	 (old-extension (file-name-extension old-path))
	 (new-extension (if (equal old-extension "ml")
			    "mli"
			    old-extension))
	 (new-path (if old-extension
		       (file-name-with-extension old-filename new-extension)
		     old-path)))
    new-path))

(defun replace-extension (old-path new-extension)
  (let* ((old-filename (file-name-sans-extension old-path))
	 (old-extension (file-name-extension old-path)))
    (unless old-extension old-filename)
    (file-name-with-extension old-filename new-extension)))

(defun validate-ml-path (path)
  (let* ((mli-path (replace-extension path "mli"))
	 (shutoffmli-path (replace-extension path "shutoffmli"))
	 (mli-path-short (file-name-nondirectory mli-path))
	 (shutoffmli-path-short (file-name-nondirectory shutoffmli-path))
	 (mli-exists (file-exists-p mli-path))
	 (shutoffmli-exists (file-exists-p shutoffmli-path)))
    (when (and mli-exists shutoffmli-exists)
      (error
       (format "%s and %s both exist; please delete one and try again."
	       mli-path-short
	       shutoffmli-path-short)))
    (when (and (not mli-exists)
	       (not shutoffmli-exists))
      (error
       (format "%s does not exist; please create it."
	       mli-path-short)))))

(defun the-file-at (path1 path2)
  (if (file-exists-p path1) path1 path2))

(defun ml-buffer-toggle ()
  (let* ((old-path buffer-file-name)
	 (mli-path (replace-extension old-path "mli"))
	 (shutoffmli-path (replace-extension old-path "shutoffmli"))
	 (mli-exists (file-exists-p mli-path))
	 (shutoffmli-exists (file-exists-p shutoffmli-path))
	 (start-path (the-file-at mli-path shutoffmli-path))
	 (new-path (transform-path start-path))
	 (start-path-short (file-name-nondirectory start-path))
	 (new-path-short (file-name-nondirectory new-path)))
    (validate-ml-path old-path)
    (when (or (file-exists-p mli-path)
	      (file-exists-p shutoffmli-path))
      (rename-file start-path new-path)
      (message
       (format "Renaming %s to %s..."
	       start-path-short
	       new-path-short)))))

(defun mli-toggle ()
  (interactive)
  (cond ((suitable-ml-buffer-p) (ml-buffer-toggle))
	((suitable-mli-buffer-p) (mli-buffer-toggle))
	((eq major-mode 'dired-mode) (mli-dired-toggle))
	(t (error "You aren't in a Tuareg buffer."))))

;; (defun sync-music ()
;;   (let* ((_ (delete-other-windows))
;; 	 (w1 (selected-window))
;; 	 (w2 (split-window-right))
;; 	 (w3 (split-window-right)))
;;     (async-shell-command "echo hi" (window-buffer w1))
;;     (async-shell-command "echo wzap" (window-buffer w2))
;;     (async-shell-command "echo dude" (window-buffer w3))))

;; (defun mt-3-thing ()
;;   (let* ((_ (delete-other-windows))
;;          (w0 (selected-window))
;;          (w1 (split-window-right))
;;          (w2 (split-window-right))
;;          (balance-windows))
;;     (async-shell-command "one of the rsyncs" (window-buffer w0))
;;     (async-shell-command "one of the rsyncs" (window-buffer w1))
;;     (async-shell-command "one of the rsyncs" (window-buffer w2))))

(defun mt-3-thing ()
  (let* ((_ (delete-other-windows))
         (w0 (selected-window))
         (cmd0 "date")
         (cmd1 "cal")
         (cmd2 "lsblk")
         (w1 (split-window-right))
         (w2 (split-window-right)))
    (balance-windows)
    (cl-loop
     for (win cmd) in `((,w0 ,cmd0) (,w1 ,cmd1) (,w2 ,cmd2))
     do (with-selected-window win
          (let ((buf (get-buffer-create (format "*%s*" cmd))))
            (display-buffer buf '(display-buffer-same-window))
            (with-current-buffer buf
              (erase-buffer)
              (async-shell-command cmd (current-buffer))))))))

;; (progn (delete-other-windows)
;;        (split-window-right)
;;        (split-window-right)
;;        (balance-windows))
