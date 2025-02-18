;; determine init code to load conditionally on hostname and os
(setq host-init (concat (car (split-string (system-name) "\\.")) "-init"))

;; load matt's personal elisp
(with-demoted-errors "%s" (load-library "fonts"))
(with-demoted-errors "%s" (load-library "fishy-prompt"))
(with-demoted-errors "%s" (load-library "toggle-gui"))

;; load init code for all hosts
(with-demoted-errors "%s" (load-library "general-init"))

;; load init code for this host
(with-demoted-errors "%s" (load-library host-init))
