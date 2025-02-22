;; get customizes outta this file
(setq custom-file "~/.emacs.d/customizes.el")
(load custom-file)

;; get ~/.emacs.d/lisp on the load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; determine init code to load conditionally on hostname and os
(setq host-init (concat (car (split-string (system-name) "\\.")) "-init"))

;; load init code for all hosts
(with-demoted-errors "%s" (load-library "general-init"))

;; load init code for this host
(with-demoted-errors "%s" (load-library host-init))
