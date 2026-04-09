;; font stuff
(defconst matt-default-font "M+ 1mn-14")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; theme
(use-package ef-themes
  :ensure t
  :defer t)

;; haskell lsp
(add-hook 'haskell-mode-hook 'eglot-ensure)

;; gui mode
(gui-mode 1)

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

;; fire up a dired buffer visiting monoconfig
(dired "~/Github/mine/monoconfig")
