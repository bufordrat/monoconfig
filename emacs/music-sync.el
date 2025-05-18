(defun music-sync ()
  (interactive)
  (let* ((_ (delete-other-windows))
         (w0 (selected-window))
         (cmd0 "pi0sync")
         (cmd1 "pi3sync")
         (cmd2 "semigroupsync")
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
