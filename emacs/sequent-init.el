;; bitmap font stuff for sequent
(defconst matt-default-font "Bok Montecarlo 12")
(add-to-list 'default-frame-alist (cons 'font matt-default-font))
(set-face-font 'default matt-default-font)

;; font toggling for sequent
(defvar matt-embiggened-yet nil)
(defun embiggen ()
  "set font for Emacs when menu-set-font can't see it"
  (interactive)
  (if matt-embiggened-yet
      (set-face-font 'default matt-default-font)
    (set-face-font 'default "Misc Fixed 20"))
  (setq matt-embiggened-yet (not matt-embiggened-yet)))
(global-set-key (kbd "<f10>") 'embiggen)

;; load pdf tools
(pdf-loader-install)

;; dired icons & doom modeline
(require 'nerd-icons)
(require 'nerd-icons-dired)
(gui-mode 1)
(setopt doom-modeline-hud t)
(setopt doom-modeline-window-width-limit 50)

;; meta key
(setopt x-super-keysym 'meta)

;; agda
(with-demoted-errors "%S"
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))

;; setopts
(setopt org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . default)))
(setopt shell-file-name "/usr/bin/zsh")
(setopt x-super-keysym 'meta)
(setopt browse-url-secondary-browser-function 'browse-url-firefox)

;; local mail stuff
(setq display-time-use-mail-icon t
      display-time-mail-string "\N{E-MAIL SYMBOL}")
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; xclip
(xclip-mode 1)

;; fire up a dired buffer visiting monoconfig
(dired "~/stuff/github/monoconfig")
