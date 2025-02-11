;; determine init code to load conditionally on hostname and os
(setq host-init (concat (car (split-string (system-name) "\\.")) "-init"))
(setq general-init (if (string-match-p "rpi" operating-system-release)
		       "general-init-nw" "general-init"))

;; load matt's personal elisp
(with-demoted-errors "%s" (load-library "fonts"))
(with-demoted-errors "%s" (load-library "fishy-prompt"))

;; load init code for all hosts
(with-demoted-errors "%s" (load-library general-init))

;; load init code for this host
(with-demoted-errors "%s" (load-library host-init))
