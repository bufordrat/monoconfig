(setq gnus-select-method '(nnnil ""))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      ;; number of days
		      (nnmail-expiry-wait immediate)))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "outlook"
                      (nnimap-address "outlook.office365.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ;; number of days
		      (nnmail-expiry-wait 14)))
(setq gnus-extra-headers '(To Newsgroups X-GM-LABELS))
(setq gnus-show-threads nil)
(defun kw-gnus-summary-hide-article ()
  "Hide the selected article."
  (interactive)
  (delete-windows-on gnus-article-buffer))
(defun kw-expert-B-del (&optional n)
  (interactive "P" gnus-summary-mode)
  (let ((gnus-novice-user nil))
    (gnus-summary-delete-article n)))
(defun kw-actually-delete-for-real (&optional n)
  (interactive "P" gnus-summary-mode)
  (let ((gnus-novice-user nil))
    (gnus-summary-move-article n "nnimap+gmail:[Gmail]/Trash")))
(with-eval-after-load 'gnus-sum
  (define-key gnus-summary-mode-map (kbd "z") #'kw-gnus-summary-hide-article)
  (define-key gnus-summary-mode-map (kbd "C-c a") #'kw-expert-B-del)
  (define-key gnus-summary-mode-map (kbd "<delete>") #'kw-actually-delete-for-real))
(setq gnus-summary-line-format "%6N %U%R%z%I%(%[%&user-date; %{%-20,20f%}%] %*%S%)\n")
(setq gnus-message-archive-group '("nnimap+gmail:[Gmail]/Sent Mail"))
(setq gnus-inhibit-images t)            ;web bugs...
(setq mm-discouraged-alternatives '("text/html" "image/.*"))
(add-hook 'gnus-summary-prepared-hook #'gnus-summary-insert-old-articles)
(setq shr-image-animate nil)
(setq shr-color-visible-luminance-min 70)
(setq shr-width 84)                     ;shr's fill-column, effectively
(setq shr-use-fonts nil)                ;no proportional fonts please!
(setq shr-use-colors nil)

(defun matt-gnus-summary-scroll-down (lines)
  (interactive "p" gnus-summary-mode)
  (gnus-summary-scroll-up (- lines)))
(define-key gnus-summary-mode-map (kbd "S-<return>") #'matt-gnus-summary-scroll-down)

(setq gnus-mime-display-multipart-related-as-mixed t) ; display multipart related as mixed
(setq gnus-article-loose-mime t)	; don't require MIME-Version:
(setq gnus-inhibit-mime-unbuttonizing t) ; I want buttons (especially on multiparts)
