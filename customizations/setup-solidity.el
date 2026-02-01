;; V4 - Improved Solidity Configuration

;; --- Prerequisites for LSP ---
;; npm install -g @nomicfoundation/solidity-language-server
;; Install solc: https://docs.soliditylang.org/en/latest/installing-solidity.html

;; Configuration variables
(defcustom solidity-node-path "~/.nvm/versions/node/v23.9.0/bin"
  "Path to Node.js binaries."
  :type 'string
  :group 'solidity)

(defcustom solidity-solc-path "/home/filip/.cargo/bin/solc"
  "Path to solc compiler."
  :type 'string
  :group 'solidity)

;; Ensure required packages are available
(defun solidity-check-dependencies ()
  "Check if required Solidity tools are available."
  (unless (executable-find "solc")
    (message "Warning: solc not found in PATH"))
  (unless (file-exists-p (expand-file-name "nomicfoundation-solidity-language-server" solidity-node-path))
    (message "Warning: solidity-language-server not found")))

;; Core Solidity mode setup
(use-package solidity-mode
  :mode "\\.sol\\'"
  :hook ((solidity-mode . solidity-mode-setup)
         (solidity-mode . flycheck-mode)
         (solidity-mode . lsp-deferred))
  :config
  (solidity-check-dependencies))

;; Company completion
(use-package company-solidity
  :after (company solidity-mode)
  :config
  (add-to-list 'company-backends 'company-solidity))

;; LSP configuration
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; (lsp-solidity-server-command `(,(expand-file-name "vscode-solidity-server" solidity-node-path) "--stdio"))
  ;; (lsp-solidity-server-command `(,(expand-file-name "solidity-language-server" solidity-node-path) "--stdio"))
  (lsp-solidity-server-command `(,(expand-file-name "nomicfoundation-solidity-language-server" solidity-node-path) "--stdio"))
  :config
  (add-to-list 'exec-path solidity-node-path))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t))

;; Flycheck for syntax checking
(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package solidity-flycheck
  :after (solidity-mode flycheck)
  :config
  (setq solidity-flycheck-solc-checker-active t
        solidity-flycheck-chaining-error-level t
        solidity-flycheck-use-project t))

;; Custom setup function
(defun solidity-mode-setup ()
  "Custom configurations for Solidity mode."
  ;; Disable inlay hints (not supported by solidity-language-server)
  (setq-local lsp-inlay-hint-enable nil)

  ;; Company backends
  (setq-local company-backends '((company-solidity company-dabbrev-code)))

  ;; Flycheck settings
  (setq-local flycheck-solidity-solc-addstd-contracts t)

  ;; Compiler paths
  (setq solidity-solc-path solidity-solc-path)

  ;; Whitespace cleanup
  (setq-local whitespace-style '(face trailing tabs spaces))
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)

  ;; Optional: Paredit for structured editing
  (when (fboundp 'paredit-mode)
    (paredit-mode 1)))

;; Disable inlay hints after LSP starts for solidity buffers
(add-hook 'lsp-after-open-hook
          (lambda ()
            (when (derived-mode-p 'solidity-mode)
              (lsp-inlay-hints-mode -1))))

;; Project-specific working directory for flycheck
(defun solidity-flycheck-working-directory ()
  "Find project root for Solidity flycheck."
  (or ;;(locate-dominating-file buffer-file-name "hardhat.config.ts")
      ;;(locate-dominating-file buffer-file-name "hardhat.config.js")
      (locate-dominating-file buffer-file-name "foundry.toml")
      (locate-dominating-file buffer-file-name "package.json")
      default-directory))

(provide 'setup-solidity)

