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
       :desc "Toggle vterm split"             "v" #'+vterm/toggle
        (:prefix ("p" . "PDF")
         :desc "Toggle Dark Mode"              "d" #'pdf-view-midnight-minor-mode
         :desc "Toggle Printed Mode"           "p" #'pdf-view-printer-minor-mode)))


(map! :leader
      (:prefix ("o" . "open here")
       :desc "Open vterm here"     "v" #'+vterm/here))

(map! "<f5>"
      :desc "Compile" #'compile)

(map! :leader
      (:prefix ("e" . "Eglot")
       :desc "Code Actions" "a" #'eglot-code-actions
       :desc "Format Buffer" "f" #'eglot-format-buffer
       :desc "Start Eglot" "s" #'eglot))

(map! :leader
      (:prefix ("g" . "git")
       :desc "Magit Push" "p" #'magit-push))

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

(after! exec-path-from-shell
  (when (memq window-system '(mac ns))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
    (exec-path-from-shell-initialize)))

(setq org-directory "~/org/")

(add-hook 'org-mode-hook #'org-modern-mode)

(setq org-modern-table-vertical 1)
(setq org-modern-table t)

(add-hook 'org-mode-hook #'hl-todo-mode)

(after! org
  (setq org-startup-with-latex-preview t))

(after! org
  (setq org-latex-create-formula-image-program 'dvisvgm))

(after! org
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.0)))

(after! org
  (setq org-latex-pdf-process
        '("latexmk -pdf -pdflatex=\"lualatex -interaction=nonstopmode -synctex=1 %O %S\" -use-make -f %f")))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(after! eglot
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer"))))

(defvar programmingLanguages
  '(("java" . "java")
    ("c" . "c")
    ("cpp" . "cpp")
    ("python" . "py")
    ("javascript" . "js")
    ("javascriptreact" . "jsx")
    ("typescript" . "ts")
    ("typescriptreact" . "tsx")
    ("rust" . "rust")
    ("html" . "html")
    ("css" . "css")
    ("swift" . "swift")
    ("emacs-lisp" . "el")
    ("sql" . "sql")))

(setq-default indent-tabs-mode nil) ; keine Tabs, nur Spaces
(setq-default tab-width 2)
(setq-default standard-indent 2)

(add-hook 'prog-mode-hook (lambda ()
                            (setq-local indent-tabs-mode nil) ; keine Tabs, nur Spaces
                            (setq-local tab-width 2)
                            (setq-local standard-indent 2)))

(add-hook 'python-mode-hook (lambda () (setq-local python-indent-offset 2)))
(add-hook 'js-mode-hook     (lambda () (setq-local js-indent-level 2)))
(add-hook 'typescript-mode-hook (lambda () (setq-local typescript-indent-level 2)))
(add-hook 'c-mode-hook      (lambda () (setq-local c-basic-offset 2)))
(add-hook 'c++-mode-hook    (lambda () (setq-local c-basic-offset 2)))
(add-hook 'rust-mode-hook   (lambda () (setq-local rust-indent-offset 2)))
(add-hook 'swift-mode-hook  (lambda () (setq-local swift-indent-offset 2)))
(add-hook 'java-mode-hook   (lambda () (setq-local c-basic-offset 2)))
(add-hook 'html-mode-hook   (lambda () (setq-local sgml-basic-offset 2)))
(add-hook 'web-mode-hook   (lambda () (setq-local web-mode-markup-indent-offset 2)))
(add-hook 'css-mode-hook    (lambda () (setq-local css-indent-offset 2)))
(add-hook 'elisp-mode-hook  (lambda () (setq-local lisp-indent-offset 2)))
(add-hook 'org-mode-hook    (lambda () (setq-local org-indent-offset 2)))

(after! copilot
  (setq copilot-indent-offset-warning-disable t)

  (setq copilot-major-mode-alist
        '((python-mode . 2)
          (js-mode . 2)
          (typescript-mode . 2)
          (c-mode . 2)
          (c++-mode . 2)
          (rust-mode . 2)
          (swift-mode . 2)
          (java-mode . 2)
          (html-mode . 2)
          (css-mode . 2)
          (emacs-lisp-mode . 2)
          (org-mode . 2)))
)

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))

(add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

(add-hook 'pdf-view-mode-hook #'pdf-view-printer-minor-mode)

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
  (defun mahd/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (indent-for-tab-command))) ; Default action to insert a tab. Adjust as needed.

  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") 'mahd/copilot-tab-or-default))

;; (copilot-on-request
;;  'window/showMessageRequest
;;  (lambda (msg) (notifications-notify :title "Emacs Copilot" :body (plist-get msg :message))))

;; (copilot-on-notification
;;   'window/logMessage
;;   (lambda (msg) (message (plist-get msg :message))

(after! copilot
  (dolist (pair programmingLanguages)
    (add-to-list 'copilot-major-mode-alist pair)))

(after! copilot
  (setq copilot-enable-predicates '(evil-insert-state-p)))

(after! copilot
  (setq copilot-enable-display-predicates '(copilot-display-predicate-evil-insert)))

(add-hook 'rustic-mode-hook
          (lambda () (eglot-ensure)))

(after! tex
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t            ; PDF statt DVI
        ;; Standard-Engine: luatex für Unicode, Systemfonts & Lua-Erweiterbarkeit
        TeX-engine 'luatex
        ;; Quelle<->PDF Synchronisation via SyncTeX
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t))

(after! tex
  ;; Entferne alte LatexMk-Definition falls vorhanden
  (setq TeX-command-list (assoc-delete-all "LatexMk" TeX-command-list))

  ;; Füge LatexMk mit LuaLaTeX hinzu
  (add-to-list 'TeX-command-list
               '("LatexMk" "latexmk -pdf -pdflatex=\"lualatex -interaction=nonstopmode -synctex=1 %%O %%S\" %s"
                 TeX-run-TeX nil t :help "Run LatexMk with LuaLaTeX"))

  (setq TeX-command-default "LatexMk"))

(after! tex
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

(after! tex
  (add-hook 'LaTeX-mode-hook #'company-mode))

(after! tex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))))

(after! tex
  ;; Weitere Engine-Optionen verfügbar machen
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "latexmk -pdf -pdflatex=\"xelatex -interaction=nonstopmode -synctex=1 %%O %%S\" %s"
                 TeX-run-TeX nil t :help "Run LatexMk with XeLaTeX"))

  (add-to-list 'TeX-command-list
               '("pdfLaTeX" "latexmk -pdf -pdflatex=\"pdflatex -interaction=nonstopmode -synctex=1 %%O %%S\" %s"
                 TeX-run-TeX nil t :help "Run LatexMk with pdfLaTeX")))

(map! :map LaTeX-mode-map
      :localleader
      :desc "View PDF" "v" #'TeX-view
      :desc "Compile" "c" #'TeX-command-master
      :desc "Clean" "k" #'TeX-clean
      :desc "Insert Macro" "m" #'TeX-insert-macro
      :desc "Insert Environment" "e" #'LaTeX-environment
      :desc "Close Environment" "E" #'LaTeX-close-environment
      :desc "Insert Section" "s" #'LaTeX-section
      :desc "RefTeX citation" "r c" #'reftex-citation
      :desc "RefTeX reference" "r r" #'reftex-reference
      :desc "RefTeX label" "r l" #'reftex-label
      :desc "RefTeX TOC" "r t" #'reftex-toc)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq-local LaTeX-indent-level 2)
                             (setq-local LaTeX-item-indent 0)))

(after! copilot
  (add-to-list 'copilot-major-mode-alist '("latex" . "tex")))

(when (memq window-system '(mac ns))
  (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
  (add-to-list 'exec-path "/Library/TeX/texbin"))
