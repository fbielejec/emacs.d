;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

;; (use-package rustic
;;   :ensure t
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status)
;;               ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
;;               ("C-c C-c d" . dap-hydra)
;;               ("C-c C-c h" . lsp-ui-doc-glance))
;;   :config
;;   ;; uncomment for less flashiness
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-enable-symbol-highlighting t)
;;   (setq rustic-format-on-save t)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; paredit
;;   (add-hook 'rustic-mode-hook 'enable-paredit-mode)

;;   ;; rustfmt on save
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)

;;   )

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hooks nil)  ; Disable eldoc
  (setq lsp-enable-symbol-highlighting t)
  (setq rustic-format-on-save t)
  ;; (setq lsp-signature-auto-activate nil)

  ;; Enable paredit and custom hook function
  (add-hook 'rustic-mode-hook
            (lambda ()
              (enable-paredit-mode)
              (rk/rustic-mode-hook)))
  )

;; rust fmt on save
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; ;; paredit
;; (add-hook 'rustic-mode-hook 'enable-paredit-mode)

;; ;; paredit again
;; (add-hook 'rustic-mode-hook (lambda () (paredit-mode 1)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Will require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-inlay-hint-enable t)
  ;; or "always"
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  ;; or t
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; or t
  (lsp-rust-analyzer-display-parameter-hints nil)
  ;; or "always"
  (lsp-rust-analyzer-display-reborrow-hints nil)

  ;; compile with all features on
  ;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-rust.el#L196
  (lsp-rust-all-features t)
  ;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-rust.el#L188C1-L189C1
  (lsp-rust-features "all")

  :config
  ;; comment to remove UI mode
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#inlay-hints
  (add-hook 'lsp-mode-hook 'lsp-inlay-hints-mode)
  ;; bump up the chunk-processing threshold to allow for a smoother Emacs experience.
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  )

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck :ensure)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))

  ;; :config
  ;; ;; turn off company for magit (it remaps Tab)
  ;; (setq company-global-modes '(not magit-mode))
  ;; (setq company-global-modes '(not org-mode))
  ;; (with-eval-after-load 'magit-mode
  ;;   (define-key company-mode-map (kbd "<Tab>") nil))
  )

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Create / cleanup rust scratch projects quickly

(use-package rust-playground :ensure)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)
