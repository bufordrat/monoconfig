;; get ~/.emacs.d/lisp and ~/.emacs.d/agda on the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; load init code for all hosts
(with-demoted-errors "%s" (load-library "basics"))

;; get customizes outta this file
(setq custom-file "~/.emacs.d/customizes.el")
(load custom-file)
