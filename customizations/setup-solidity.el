;;;;
;; Solidity
;;;;

;; for syntax checking in solidity-mode
(setq solidity-solc-path "/usr/bin/solc")

(use-package flycheck
  :ensure t
  :init (solidity-mode))
