;; font stuff
(defconst matt-default-font "M+ 1mn-18")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; dired icons & doom modeline
(require 'nerd-icons)
(require 'nerd-icons-dired)
(gui-mode 1)
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

;; agda
(with-demoted-errors "%S"
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; xclip
(xclip-mode 1)

;; fire up a dired buffer visiting monoconfig
(dired "~/github/mine/monoconfig")
