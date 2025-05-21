(defun bastion ()
  (interactive)
  (async-shell-command "ssh -fND localhost:2200 -J stax sequent"))
