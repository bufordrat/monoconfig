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

(defun update-buffer-names (old new)
  (when-let ((buf (get-file-buffer old)))
    (if-let ((win (get-buffer-window buf)))
        (with-current-buffer buf
          (with-selected-window win
            (find-alternate-file new)))
      (let ((config (current-window-configuration)))
        (with-current-buffer buf
          (find-alternate-file new)
          (set-window-configuration config))))))

(defun mli-dired-toggle ()
  (interactive)
  (when (dired-buffer-stale-p)
    (error "Your dired buffer is stale; please revert."))
  (let* ((old-path (dired-get-filename))
	 (new-path (transform-path old-path)))
    (unless (mli-p old-path)
      (error "Not an .mli file."))
    (rename-file old-path new-path)
    (update-buffer-names old-path new-path)
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

(defun refresh-relevant-direds ()
  (cl-loop
   with current-dir = (expand-file-name default-directory)
   for buf being the buffers
   do (with-current-buffer buf
	(when (and (eq major-mode 'dired-mode)
		   (equal (expand-file-name default-directory)
			  current-dir))
	  (revert-buffer)))))

(defun refresh-mli-buffer (path-to-mli)
  (let ((mli-buffer (get-file-buffer (expand-file-name path-to-mli))))
    (with-current-buffer mli-buffer (mli-buffer-toggle))))

(defun mli-buffer-toggle ()
  (let* ((old-path buffer-file-name)
	 (new-path (transform-path old-path)))
    (save-buffer)
    (rename-file old-path new-path)
    (find-alternate-file new-path)
    (revert-buffer nil t)
    (refresh-relevant-direds)))

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
      (error "%s and %s both exist; please delete one and try again."
	     mli-path-short
	     shutoffmli-path-short))
    (when (and (not mli-exists)
	       (not shutoffmli-exists))
      (error "%s does not exist; please create it."
	     mli-path-short))))

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
      (update-buffer-names start-path new-path)
      (refresh-relevant-direds)
      (message "Renaming %s to %s..."
	       start-path-short
	       new-path-short))))

(defun mli-toggle ()
  (interactive)
  (cond ((suitable-ml-buffer-p) (ml-buffer-toggle))
	((suitable-mli-buffer-p) (mli-buffer-toggle))
	((eq major-mode 'dired-mode) (mli-dired-toggle))
	(t (error "You aren't in a Tuareg buffer."))))
