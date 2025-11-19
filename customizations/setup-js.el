;; ------------------------------
;; TYPESCRIPT / JAVASCRIPT SETUP
;; ------------------------------

;; Major modes
;; (use-package typescript-mode
;;   :mode "\\.ts\\'"
;;   :hook ((typescript-mode . my/ts-setup))
;;   :config
;;   (setq typescript-indent-level 2))
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook ((typescript-mode . setup-typescript-mode)
         (typescript-tsx-mode . setup-typescript-mode))
  :config
  (setq typescript-indent-level 2))

;; (use-package tsx-mode
;;   :mode "\\.tsx\\'"
;;   :hook ((tsx-mode . my/ts-setup)))

;; (use-package js-mode
;;   :ensure nil ;; built-in
;;   :mode "\\.jsx?\\'"
;;   :hook ((js-mode . my/ts-setup)))

;; Web mode for TSX/JSX files
;; (use-package web-mode
;;   :ensure t
;;   :mode ("\\.tsx\\'" "\\.jsx\\'")
;;   :hook ((web-mode . setup-web-mode-for-typescript))
;;   :config
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-code-indent-offset 2
;;         web-mode-content-types-alist '(("jsx" . "\\.tsx\\'"))))


;; LSP
;; (use-package lsp-mode
;;   :commands lsp
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil
;;         lsp-prefer-capf t
;;         lsp-eslint-auto-fix-on-save t))
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((typescript-mode . lsp)
         (typescript-tsx-mode . lsp)
         ;; (js-mode . lsp)
         ;; (js2-mode . lsp)
         ;; (web-mode . maybe-lsp-for-web-mode)
)
  :init
  (setq lsp-enable-on-type-formatting nil
        lsp-clients-typescript-server "typescript-language-server")
  :config
  (setq lsp-typescript-preferences '(:includeCompletionsForModuleExports t
                                     :includeCompletionsWithInsertText t)
        lsp-typescript-format-insert-space-after-opening-and-before-closing-jsx-expression-braces t
        lsp-typescript-format-place-open-brace-on-new-line-for-functions nil
        lsp-typescript-format-place-open-brace-on-new-line-for-control-blocks nil))

;; LSP UI
;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-header t
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-border (face-foreground 'default)
;;         lsp-ui-sideline-enable t
;;         lsp-ui-sideline-ignore-duplicate t
;;         lsp-ui-sideline-show-code-actions t
;;         lsp-ui-sideline-show-hover t
;;         lsp-ui-sideline-show-diagnostics t
;;         lsp-ui-imenu-enable t
;;         lsp-ui-imenu-kind-position 'top
;;         lsp-ui-peek-enable t
;;         lsp-ui-peek-list-width 60
;;         lsp-ui-peek-peek-height 25))


;; Company (auto-completion)
;; (use-package company
;;   :commands company-mode
;;   :config
;;   (setq company-idle-delay 0
;;         company-minimum-prefix-length 1))
(use-package company
  :ensure t
  :defer t
  :init
  ;;(push 'company-lsp company-backends)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-limit 10
        company-show-numbers t
        company-selection-wrap-around t
        )
  )

;; Flycheck
(use-package flycheck
  :ensure t
  :defer t
  :commands flycheck-mode)

(use-package yasnippet
  :ensure t
  :defer t
  :commands yas-minor-mode
  :config (yas-reload-all))

;; Paredit
(use-package paredit
  :ensure t
  :defer t
  :commands enable-paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

;; ------------------------------
;; PRETTIER SUPPORT
;; ------------------------------
;; (use-package prettier
;;   :hook ((typescript-mode . prettier-mode)
;;          (tsx-mode . prettier-mode)
;;          (js-mode . prettier-mode))
;;   :config
;;   ;; Don't format twice (LSP + Prettier)
;;   (setq prettier-mode-sync-config-flag nil))

;; ;; If user wants prettier via npx:
;; (setq prettier-pre-warm 't)

;; Prettier for code formatting
(use-package prettier-js
  :ensure t
  :defer t
  :custom
  ;; If you want prettier via npx:
  (setq prettier-pre-warm 't)
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
                           "--single-quote" "true"
                           "--print-width" 80)))


;; ------------------------------
;; OPTIONAL TREE-SITTER SUPPORT
;; ------------------------------
;; Enable if available
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (use-package tsx-ts-mode
    :ensure t
    :defer t
    :mode "\\.tsx\\'"
    :hook ((tsx-ts-mode . my/ts-setup)))

  (use-package typescript-ts-mode
    :ensure t
    :defer t
    :mode "\\.ts\\'"
    :hook ((typescript-ts-mode . my/ts-setup)))

  (use-package js-ts-mode
    :ensure t
    :defer t
    :mode "\\.jsx?\\'"
    :hook ((js-ts-mode . my/ts-setup))))


;; ------------------------------
;; MASTER HOOK: LOCAL SETUP
;; ------------------------------
;; (defun my/ts-setup ()
;;   "Setup everything for TS/JS buffers only."

;;   ;; Modes
;;   (lsp)
;;   (company-mode 1)
;;   (yas-minor-mode 1)
;;   (flycheck-mode 1)
;;   (enable-paredit-mode)

;;   ;; TAB = completion
;;   (setq-local tab-always-indent 'complete)

;;   ;; Cleanup on save
;;   (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

;;   ;; Auto-fix ESLint before save (requires lsp-eslint)
;;   (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes nil t)

;;   ;; Format with LSP unless Prettier is in use
;;   (unless (bound-and-true-p prettier-mode)
;;     (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
;; Setup functions
(defun setup-typescript-mode ()
  "Setup TypeScript development environment."
  ;; Enable completion and snippets
  (company-mode 1)
  (yas-minor-mode 1)

  ;; Enable syntax checking
  (flycheck-mode 1)

  ;; Enable structured editing
  (enable-paredit-mode)

  ;; Enable prettier formatting
  (prettier-js-mode 1)

  ;; Enable rainbow delimiters
  (rainbow-delimiters-mode 1)

  ;; Enable which-key locally
  (which-key-mode 1)

  ;; Set up formatting and whitespace cleanup on save
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)

  ;; Set indentation
  (setq tab-width 2
        indent-tabs-mode nil)

  ;; Enable emmet for TSX files
  (when (string-match-p "\\.tsx\\'" buffer-file-name)
    (emmet-mode 1))

  ;; Keybindings for TypeScript development
  (local-set-key (kbd "C-c C-f") 'lsp-format-buffer)
  (local-set-key (kbd "C-c C-r") 'lsp-rename)
  (local-set-key (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (local-set-key (kbd "C-c C-a") 'lsp-execute-code-action)
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-,") 'lsp-find-references)
  (local-set-key (kbd "C-c C-l") 'lsp-ui-imenu)
  )

;; JavaScript mode setup
;; (use-package js2-mode
;;   :ensure t
;;   :mode ("\\.js\\'")
;;   :hook (js2-mode . setup-typescript-mode))

;; (use-package js-mode
;;   :ensure t
;;   :mode ("\\.js\\'")
;;   :hook (js-mode . setup-typescript-mode))
