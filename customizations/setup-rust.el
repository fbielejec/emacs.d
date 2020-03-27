(require 'rust-mode)

;; (require 'use-package)

;; (use-package racer
;;   :requires rust-mode

;;   :init (setq racer-rust-src-path
;;               (concat (string-trim
;;                        (shell-command-to-string "rustc --print sysroot"))
;;                       "/lib/rustlib/src/rust/src"))

;;   :config
;;   (add-hook 'rust-mode-hook 'enable-paredit-mode)
;;   (add-hook 'before-save-hook 'rust-save-hook)
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'company-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;;   (setq company-tooltip-align-annotations t))

(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(defun rust-save-hook()
  (when (eq major-mode 'rust-mode)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'rust-save-hook)

;; Rustup binaries PATH
(setq racer-cmd "~/.cargo/bin/racer")

;; Rust source code PATH
(setq racer-rust-src-path
      (concat (string-trim
               (shell-command-to-string "rustc --print sysroot"))
              "/lib/rustlib/src/rust/src"))

(add-hook 'rust-mode-hook 'enable-paredit-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;; syntax checks (enable with M+x flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; autocomplete with company
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; TODO : TESTING THESE MODES

;; (use-package flycheck
;;   :hook (rust-mode . flycheck-mode))

;; (use-package company
;;   :hook (rust-mode . company-mode)
;;   :config (setq company-tooltip-align-annotations t)
;;           (setq company-minimum-prefix-length 1))

;; (use-package lsp-mode
;;   :commands lsp
;;   :config (require 'lsp-clients))

;; (use-package lsp-ui)

;; (use-package toml-mode)

;; (use-package rust-mode
;;   :hook (rust-mode . lsp))

;; Add keybindings for interacting with Cargo
;; (use-package cargo
;;   :hook (rust-mode . cargo-minor-mode))

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
