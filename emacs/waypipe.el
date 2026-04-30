(defun each-client-frame (f)
  (when (and (frame-parameter f 'environment)
	     (equal "true" (getenv "SEXP" f)))
    (delete-frame f)))

(defun zap-waypiped-client-frames ()
  (cl-loop
   for f in (frame-list)
   do (each-client-frame f)))
