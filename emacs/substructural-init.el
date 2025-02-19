;; font stuff
(defconst matt-default-font "M+ 1mn-14")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; haskell lsp
(add-hook 'haskell-mode-hook 'eglot-ensure)

;; dired icons & doom modeline
(require 'nerd-icons)
(require 'nerd-icons-dired)
(mt-turnon-gui 1)
(setopt doom-modeline-hud t)
(setopt doom-modeline-minor-modes t)
(setopt doom-modeline-window-width-limit 60)

;; agda
(with-demoted-errors "%S"
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; other setopts
(setopt display-battery-mode t)
(setopt org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "open %s")
     ("\\.pdf\\'" . default)))
(setopt shell-file-name "/bin/zsh")

;; xclip
(xclip-mode 1)
