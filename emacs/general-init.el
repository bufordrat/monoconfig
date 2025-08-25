;; load matt's personal elisp
(with-demoted-errors "%s" (load-library "fonts"))
(with-demoted-errors "%s" (load-library "fishy-prompt"))
(with-demoted-errors "%s" (load-library "toggle-gui"))
(with-demoted-errors "%s" (load-library "shells"))
(with-demoted-errors "%s" (load-library "ocaml"))
(with-demoted-errors "%s" (load-library "music-sync"))
(with-demoted-errors "%s" (load-library "bastion"))

;; cd to homedir
(cd "~")

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
(global-set-key (kbd "C-c ;") #'bastion)
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

;; temporarily turning these keybindings off for off-weaning
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-;"))

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

;; package-initialize
(setq package-enable-at-startup nil)
(package-initialize)

;; get ~/bin on the exec-path
(add-to-list 'exec-path (expand-file-name "~/bin"))

;; vlf
(require 'vlf-setup)

;; repos
(dolist (arc '(("melpa-stable" . "http://stable.melpa.org/packages/")
	       ("melpa" . "http://melpa.org/packages/")
               ("kw" . "http://www.lib.uchicago.edu/keith/software/emacs/packages/")))
  (add-to-list 'package-archives arc t))
(setq package-archive-priorities '(("kw" . 11) ("melpa-stable" . 10))) 

;; no defcustoms for these; sad
(setq default-major-mode 'fundamental-mode)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; fix ansi escape nonsense while compiling
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; completion
(use-package vertico
  :ensure t
  :config (vertico-mode))
(define-key vertico-map (kbd "RET") 'vertico-exit-input)
(define-key vertico-map (kbd "M-RET") 'vertico-exit)
(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  :init
  (marginalia-mode))
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq extended-command-suggest-shorter nil)

;; message mode
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-service 587)

;; use web-mode for html
(dolist (ext '("\\.xhtml$" "\\.htm$" "\\.html$"))
  (add-to-list 'auto-mode-alist (cons ext 'web-mode)))

;; bbdb
(with-eval-after-load 'bbdb
  (bbdb-initialize 'gnus 'message 'anniv))

;; shell mode zsh goodies
(add-hook 'shell-mode-hook
          (lambda ()
            (setq comint-input-ring-file-name "~/.zsh_history")
            (comint-read-input-ring t)
            (setq-local comint-output-filter-functions
                        '(comint-truncate-buffer
                          ansi-color-process-output
                          comint-postoutput-scroll-to-bottom
                          comint-watch-for-password-prompt))
            (setq-local comint-process-echoes t)))

;; org mode
(add-hook 'org-mode-hook 'org-indent-mode)
(require 'org-tempo)

;; python
(defvar mt-venv-path "~/envs/virtualenvs/py-default")
(pyvenv-mode +1)
(pyvenv-activate mt-venv-path)
(add-hook 'python-mode-hook 'eglot-ensure)
(defvar python-mode-map)
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "M-n") 
	      'flymake-goto-next-error)
  (define-key python-mode-map (kbd "M-p")
	      'flymake-goto-prev-error)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local parens-require-spaces nil))))

;; get ~/.local/bin on exec-path
(add-to-list 'exec-path (expand-file-name "~/.local/bin"))

;; ocaml
(when (executable-find "opam")
  (let ((opam-share (car (with-demoted-errors "%S" (process-lines "opam" "var" "share"))))
        (opam-bin   (car (with-demoted-errors "%S" (process-lines "opam" "var" "bin")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share)))
    (when (and opam-bin (file-directory-p opam-bin))
      (add-to-list 'exec-path opam-bin))))

;; get ocaml repl command to run dune from the project root
(defun tuareg-run-ocaml/in-project-root (func &rest args)
  (let ((default-directory
         (projectile-acquire-root)))
    (apply func args)))

(advice-add #'tuareg-run-ocaml :around #'tuareg-run-ocaml/in-project-root)

(autoload 'merlin-mode "merlin" nil t nil)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook
 'tuareg-mode-hook
 (lambda ()
    (keymap-set tuareg-mode-map "C-c C-c" #'projectile-compile-project)))
(add-hook 'caml-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook
          (lambda ()
            (if (locate-dominating-file default-directory "dune-project")
                (setq-local tuareg-interactive-program "opam exec -- dune exec lib/repl.exe")
              (setq-local tuareg-interactive-program "opam exec -- ocaml -nopromptcont"))))

;; haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-to-list 'exec-path (expand-file-name "~/.ghcup/bin/"))

;; tint
(dolist
    (f '(run-tint tint-mode tint-eval tint-eval-at-point))
  (autoload f "tint" nil t))

;; dired
(with-demoted-errors "%s" (diredfl-global-mode +1))

;; setq-default-s
(setq-default truncate-lines t)

;; setq-s
;;  (these used to be customizes, but were moved in here; see other .el
;;   files for more domain-specific setq-s/setopts)
(setq all-the-icons-dired-monochrome nil)
(setq backup-directory-alist '(("." . "~/.squiggles")))
(setq bbdb-file "~/bbdb/bbdb")
(setq blink-cursor-mode nil)
(setq browse-url-browser-function 'eww-browse-url)
(setq comint-buffer-maximum-size 65336)
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(setq diff-switches "-u")
(setq dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq enable-recursive-minibuffers t)
(setq epg-pinentry-mode 'loopback)
(setq magit-clone-set-remote.pushDefault t)
(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq proced-enable-color-flag t)
(setq ring-bell-function 'ignore)
(setq tooltip-mode nil)
(setq truncate-partial-width-windows nil)
(setq tuareg-opam-insinuate t)
(setq auto-save-default nil)
