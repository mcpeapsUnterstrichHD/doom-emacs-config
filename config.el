(setq user-full-name "mcpeapsUnterstrichHD"
      user-mail-address "mcpeaps_HD@outlook.com")

(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :size 18)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :size 24)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 18)
      doom-serif-font (font-spec :family "CaskaydiaCove Nerd Font" :size 18)
      doom-variable-pitch-font (font-spec :family "CaskaydiaCove Nerd Font Propo" :size 18))

(add-hook 'vterm-mode-hook
  (lambda ()
    (buffer-face-set '(:family "CaskaydiaCove Nerd Font Mono" :size 18))))

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.1)))))

(defun mahd/toggle-markdown-view-mode ()
  "Toggle between `markdown-mode' and `markdown-view-mode'."
  (interactive)
  (if (eq major-mode 'markdown-view-mode)
      (markdown-mode)
    (markdown-view-mode)))

(custom-theme-set-faces!
  'doom-nord
  '(org-level-8 :inherit outline-3 :height 1.0)
  '(org-level-7 :inherit outline-3 :height 1.0)
  '(org-level-6 :inherit outline-3 :height 1.1)
  '(org-level-5 :inherit outline-3 :height 1.2)
  '(org-level-4 :inherit outline-3 :height 1.3)
  '(org-level-3 :inherit outline-3 :height 1.4)
  '(org-level-2 :inherit outline-2 :height 1.5)
  '(org-level-1 :inherit outline-1 :height 1.6)
  '(org-document-title :height 1.8 :bold t :underline nil)
  )

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type 'relative)

(map! :leader
      :desc "Line Comments" "-" #'comment-line)

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers"            "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle markdown-view-mode"      "m" #'mahd/toggle-markdown-view-mode
       :desc "Toggle truncate lines"          "t" #'toggle-truncate-lines
       :desc "Toggle treemacs"                "T" #'+treemacs/toggle
       :desc "Toggle vterm split"             "v" #'+vterm/toggle))

(map! :leader
      (:prefix ("o" . "open here")
       :desc "Open vterm here"     "v" #'+vterm/here))

(after! doom-start
(blink-cursor-mode 1))

;; (after! evil
;;   (blink-cursor-mode 1))

;; (add-hook 'emacs-startup-hook
;;           (lambda () (run-with-idle-timer 0.5 nil (lambda () (blink-cursor-mode 1)))))

(after! doom-start
(setq blink-cursor-delay 0.5))

(after! doom-start
(setq blink-cursor-interval 0.5))

(setq org-directory "~/org/")

(setq org-modern-table-vertical 1)
(setq org-modern-table t)

(add-hook 'org-mode-hook #'hl-todo-mode)

(add-hook 'pdf-view-mode-hook #'(lamda () (interactive) (display-line-numbers-mode -1)))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :hook (org-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))

;; (copilot-on-request
;;  'window/showMessageRequest
;;  (lambda (msg) (notifications-notify :title "Emacs Copilot" :body (plist-get msg :message))))

;; (copilot-on-notification
;;   'window/logMessage
;;   (lambda (msg) (message (plist-get msg :message))
