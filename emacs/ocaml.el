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
	 (mli-exists (file-exists-p mli-path))
	 (shutoffmli-exists (file-exists-p shutoffmli-path)))
    (when (and mli-exists shutoffmli-exists)
      (error
       ".mli and .shutoffmli correspondents of this file both exist; please
delete one and try again."))
    (when (and (not mli-exists)
	       (not shutoffmli-exists))
      (error "This .ml file has no .mli correspondent."))))

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
