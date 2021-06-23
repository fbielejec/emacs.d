;;; --- CONFIG 2

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; rustic = basic rust-mode + additions

(use-package rustic
  :ensure
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
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (add-hook 'rustic-mode-hook 'enable-paredit-mode)
  )

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

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
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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
        ("TAB". tab-indent-or-complete)))

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
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;; TODO : WIP
(when (executable-find "lldb-mi")
  (use-package dap-mode
    :ensure
    :config
    (dap-ui-mode)
    (dap-ui-controls-mode 1)

    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    ;; installs .extension/vscode
    (dap-gdb-lldb-setup)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
           :gdbpath "rust-lldb"
           ;; uncomment if lldb-mi is not in PATH
           ;; :lldbmipath "path/to/lldb-mi"
           :lldbmipath "/usr/local/bin/lldb-mi"
           ))))

(add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))

;;; --- END: CONFIG 2

;;; --- CONFIG 1

;; (require 'rust-mode)

;; ;; (require 'use-package)

;; ;; (use-package racer
;; ;;   :requires rust-mode

;; ;;   :init (setq racer-rust-src-path
;; ;;               (concat (string-trim
;; ;;                        (shell-command-to-string "rustc --print sysroot"))
;; ;;                       "/lib/rustlib/src/rust/src"))

;; ;;   :config
;; ;;   (add-hook 'rust-mode-hook 'enable-paredit-mode)
;; ;;   (add-hook 'before-save-hook 'rust-save-hook)
;; ;;   (add-hook 'rust-mode-hook #'racer-mode)
;; ;;   (add-hook 'racer-mode-hook #'company-mode)
;; ;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;; ;;   (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; ;;   (setq company-tooltip-align-annotations t))

;; (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; (defun rust-save-hook()
;;   (when (eq major-mode 'rust-mode)
;;     (whitespace-cleanup)))

;; (add-hook 'before-save-hook 'rust-save-hook)

;; ;; Rustup binaries PATH
;; (setq racer-cmd "~/.cargo/bin/racer")

;; ;; Rust source code PATH
;; (setq racer-rust-src-path
;;       (concat (string-trim
;;                (shell-command-to-string "rustc --print sysroot"))
;;               "/lib/rustlib/src/rust/src"))

;; (add-hook 'rust-mode-hook 'enable-paredit-mode)
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)

;; ;; autocomplete with company
;; (add-hook 'racer-mode-hook #'company-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; ;; TODO : TESTING THESE MODES

;; ;; syntax checks (enable with M+x flycheck-mode)
;; ;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; ;; (use-package flycheck
;; ;;   :hook (rust-mode . flycheck-mode))

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; ;; (use-package company
;; ;;   :hook (rust-mode . company-mode)
;; ;;   :config (setq company-tooltip-align-annotations t)
;; ;;           (setq company-minimum-prefix-length 1))

;; ;; (use-package lsp-mode
;; ;;   :commands lsp
;; ;;   :config (require 'lsp-clients))

;; ;; (use-package lsp-ui)

;; ;; (use-package toml-mode)

;; ;; (use-package rust-mode
;; ;;   :hook (rust-mode . lsp))

;; ;; Add keybindings for interacting with Cargo
;; ;; (use-package cargo
;; ;;   :hook (rust-mode . cargo-minor-mode))

;; ;; (use-package flycheck-rust
;; ;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; --- END:  CONFIG 1
