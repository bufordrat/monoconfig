;; sly
(use-package sly
  :ensure t
  :defer t
  :init (setq inferior-lisp-program "/usr/bin/sbcl"))

;; font stuff
(defconst matt-default-font "M+ 1mn-12")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; theme
(use-package gruvbox-theme
  :ensure t
  :defer t)

;; gui mode
(gui-mode 1)

;; meta key
(setopt x-super-keysym 'meta)

;; agda
(with-demoted-errors "%S"
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; other setopts
(setopt org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . default)))
(setopt shell-file-name "/usr/bin/zsh")

;; local mail stuff
(setq display-time-use-mail-icon nil
      display-time-mail-string "\N{OPEN MAILBOX WITH RAISED FLAG}"
      display-time-mail-directory (expand-file-name "~/.maildir/local_mail/new"))
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; fire up a dired buffer visiting monoconfig
(dired "~/Stuff/GitHub/mine/monoconfig/")

;; haskell lsp
;; Thu Nov  6 09:21:28 AM CST 2025
;; commenting this out because it's broken for the moment
;; need to fix
;; (add-hook 'haskell-mode-hook 'eglot-ensure)
