;; setopts
(setq org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . default)))
(setq shell-file-name "/usr/bin/zsh")
(setq x-super-keysym 'meta)

;; xclip
(xclip-mode 1)

;; fire up a dired buffer visiting monoconfig
(dired "~/stuff/github/monoconfig/")

;; no icons and stuff for pitype
(gui-mode 0)
