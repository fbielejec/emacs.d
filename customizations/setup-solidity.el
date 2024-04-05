;; V2

(require 'solidity-mode)
(setq solidity-comment-style 'slash)
(setq solidity-solc-path "/home/filip/.nvm/versions/node/v18.15.0/bin/solcjs")
(setq solidity-solium-path "/home/filip/.nvm/versions/node/v18.15.0/bin/solium")

(setq flycheck-solidity-solium-soliumrcfile "/home/filip/.soliumrc.json")

(define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point)

(require 'solidity-flycheck)
;; (setq solidity-flycheck-solc-checker-active t)
(setq solidity-flycheck-solium-checker-active t)

;; (setq flycheck-solidity-solc-addstd-contracts t)

(require 'company-solidity)

;; remove whitespace on save
(when (eq major-mode 'solidity-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup))

;; V1

;; (use-package solidity-mode
;;   :ensure t
;;   :config
;;   (setq solidity-solc-path "/home/filip/.nvm/versions/node/v18.15.0/bin/solcjs")
;;   (setq solidity-solium-path "/home/filip/.nvm/versions/node/v18.15.0/bin/solium")

;;   (setq solidity-flycheck-solc-checker-active t)
;;   (setq solidity-flycheck-solium-checker-active t)

;;   (setq flycheck-solidity-solc-addstd-contracts t)
;;   (setq flycheck-solidity-solium-soliumrcfile "~/.soliumrc.json")

;;   (setq solidity-comment-style 'slash))
