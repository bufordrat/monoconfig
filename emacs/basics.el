;; keybindings
(global-set-key (kbd "C-c v") #'visual-line-mode)
(global-set-key (kbd "C-c f p") #'mt-change-font-family)
(global-set-key (kbd "C-c f h") #'mt-change-font-size)
(global-set-key (kbd "C-c f n") #'mt-hires-sequent-client)
(global-set-key (kbd "C-c g") #'gui-mode)
(global-set-key (kbd "C-c \\") #'mt-shell)
(global-set-key (kbd "C-c |") #'eshell)
(global-set-key (kbd "C-c m") #'magit-clone)
(global-set-key (kbd "C-c i") #'mli-toggle)
(global-set-key (kbd "M-+") #'scroll-up-line)
(global-set-key (kbd "M-_") #'scroll-down-line)
(global-set-key (kbd "M-3") #'split-window-right)
(global-set-key (kbd "M-2") #'split-window-below)
(global-set-key (kbd "M-0") #'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)

;; replacements for GUI Emacs keybindings
(global-set-key (kbd "M-;") #'comment-line)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-x ,") #'previous-buffer)
(global-set-key (kbd "C-x .") #'next-buffer)
(global-set-key (kbd "C-c 9") #'winner-undo)
(global-set-key (kbd "C-c 0") #'winner-redo)

;; function calls
(winner-mode 1)
(windmove-default-keybindings)
(size-indication-mode 1)
(display-time)
(show-paren-mode 1)

;; menu bar stuff
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; column numbers
(column-number-mode 1)

;; crucial setq-s
(setq backup-directory-alist '(("." . "~/.squiggles")))
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq auto-save-default nil)
(setq enable-recursive-minibuffers t)
(setq blink-cursor-mode nil)
(setq epg-pinentry-mode 'loopback)
(setq ring-bell-function 'ignore)
(setq tooltip-mode nil)
(setq truncate-partial-width-windows nil)
