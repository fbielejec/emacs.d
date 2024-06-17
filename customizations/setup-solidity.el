;; V3

;; --- Prerequisite for LSP
;; npm install -g solidity-language-server solc solium
;; sudo rm -f /usr/bin/node
;; sudo rm -f /usr/bin/npm
;; sudo ln -s $(which node) /usr/bin/
;; sudo ln -s $(which npm) /usr/bin/

;; Enable Solidity mode for .sol files
(require 'solidity-mode)
(add-to-list 'auto-mode-alist '("\\.sol$" . solidity-mode))

;; Flycheck configuration for Solidity
(require 'flycheck)
(add-hook 'solidity-mode-hook 'flycheck-mode)

;; Company mode configuration
(require 'company)
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
  (setq-local flycheck-solidity-solium-soliumrcfile "/home/filip/.soliumrc.json") ; Update this path if you have a solium config file
  (setq-local flycheck-solidity-solc-addstd-contracts t)

  ;; to find npm
  (setq exec-path (append exec-path '("~/.nvm/versions/node/v18.19.0/bin")))
  (setq solidity-solc-path "/home/filip/.nvm/versions/node/v18.19.0/bin/solcjs")
  (setq solidity-solium-path "/home/filip/.nvm/versions/node/v18.19.0/bin/solium")

  ;; Whitespace cleanup on save
  (setq whitespace-style '(face trailing tabs spaces))
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)

  )

(add-hook 'solidity-mode-hook 'my-solidity-mode-setup)

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

(provide 'init-solidity)
