;; V3

;; --- Prerequisite for LSP
;; npm install -g solidity-language-server solium
;; solc
;; https://github.com/ethereum/solc-bin/blob/gh-pages/linux-amd64/solc-linux-amd64-v0.8.26%2Bcommit.8a97fa7a
;; sudo ln -s -f /home/filip/Programs/solc-linux-amd64 /usr/local/bin/solc

;; Enable Solidity mode for .sol files
(require 'solidity-mode)
(add-to-list 'auto-mode-alist '("\\.sol$" . solidity-mode))

;; Company mode configuration
(require 'company)
(require 'company-solidity)
;; (add-hook 'after-init-hook 'global-company-mode)

;; Paredit mode configuration
(require 'paredit)
(add-hook 'solidity-mode-hook 'paredit-mode)

;; Whitespace cleanup on save
;; (require 'whitespace)
;; (setq whitespace-style '(face trailing tabs spaces))

;; LSP mode configuration
(require 'lsp-mode)
(add-hook 'solidity-mode-hook #'lsp)

;; LSP UI for better user interface
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; Local variables
(defun my-solidity-mode-setup ()
  "Custom configurations for Solidity mode."
  (setq-local company-backends '((company-dabbrev-code)))
  ;; (setq-local flycheck-solidity-solium-soliumrcfile "/home/filip/.soliumrc.json") ; Update this path if you have a solium config file
  (setq-local flycheck-solidity-solc-addstd-contracts t)

  ;; @filip experimental
  ;; (setq solidity-flycheck-solium-checker-active nil)
  (setq solidity-flycheck-solc-checker-active t)
  (setq solidity-flycheck-chaining-error-level t)
  (setq solidity-flycheck-use-project t)
  ;; (setq solidity-flycheck-solc-additional-allow-paths '("/home/filip/CloudStation/aleph/my-lz-oapp/node_modules/"))

  ;; to find npm
  (setq exec-path (append exec-path '("~/.nvm/versions/node/v18.19.0/bin")))
  ;; (setq solidity-solc-path "/home/filip/.nvm/versions/node/v18.19.0/bin/solcjs")
  (setq solidity-solc-path "/usr/local/bin/solc")
  (setq solidity-solium-path "/home/filip/.nvm/versions/node/v18.19.0/bin/solium")

  ;; Whitespace cleanup on save
  (setq whitespace-style '(face trailing tabs spaces))
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)

  ;; (define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point)

  )

(require 'solidity-flycheck) ;; TMP: https://github.com/ethereum/emacs-solidity/issues/41#issuecomment-508982585
(add-hook 'solidity-mode-hook 'my-solidity-mode-setup)

;; Flycheck configuration for Solidity
(require 'flycheck)
(add-hook 'solidity-mode-hook 'flycheck-mode)


;; (use-package solidity-flycheck
;;   :defer t
;;   :init

;;   (setq-local company-backends '((company-dabbrev-code)))
;;   ;; (setq-local flycheck-solidity-solium-soliumrcfile "/home/filip/.soliumrc.json") ; Update this path if you have a solium config file
;;   (setq-local flycheck-solidity-solc-addstd-contracts t)

;;   ;; @filip experimental
;;   ;; (setq solidity-flycheck-solium-checker-active nil)
;;   (setq solidity-flycheck-solc-checker-active t)
;;   (setq solidity-flycheck-chaining-error-level t)
;;   (setq solidity-flycheck-use-project t)
;;   ;; (setq solidity-flycheck-solc-additional-allow-paths '("/home/filip/CloudStation/aleph/my-lz-oapp/node_modules/"))

;;   ;; to find npm
;;   (setq exec-path (append exec-path '("~/.nvm/versions/node/v18.19.0/bin")))
;;   ;; (setq solidity-solc-path "/home/filip/.nvm/versions/node/v18.19.0/bin/solcjs")
;;   (setq solidity-solc-path "/usr/local/bin/solc")
;;   (setq solidity-solium-path "/home/filip/.nvm/versions/node/v18.19.0/bin/solium")

;;   (add-hook
;;    'solidity-mode-hook
;;    (lambda ()
;;      (require 'solidity-flycheck))))

;; Whitespace cleanup on save
;; (setq whitespace-style '(face trailing tabs spaces))
;; (add-hook 'before-save-hook 'whitespace-cleanup nil t)

;; Additional configurations (optional)
;; (setq company-idle-delay 0.2)
;; (setq company-minimum-prefix-length 1)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;; Optional: LSP related settings
;; (setq lsp-enable-snippet t)
;; (setq lsp-enable-symbol-highlighting t)
;; (setq lsp-ui-doc-enable t)
;; (setq lsp-ui-imenu-enable t)
;; (setq lsp-ui-sideline-enable t)
;; (setq lsp-ui-flycheck-enable t)

;; (provide 'init-solidity)

;; (defun solidity-flycheck-working-directory ()
;;   (locate-dominating-file buffer-file-name "hardhat.config.ts"))
