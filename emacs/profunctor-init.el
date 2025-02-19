;; font stuff
(defconst matt-default-font "M+ 1mn-18")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; dired icons & doom modeline
(require 'nerd-icons)
(require 'nerd-icons-dired)
(mt-turnon-gui 1)
(setopt doom-modeline-hud t)
(setopt doom-modeline-minor-modes t)
(setopt doom-modeline-window-width-limit 60)

;; setopts
(setopt org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "open %s")
     ("\\.pdf\\'" . default)))
(setopt shell-file-name "/bin/zsh")

;; xclip
(xclip-mode 1)
