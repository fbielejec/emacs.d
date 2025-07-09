;; PREREQUISITES
;;
;; compiler: solc
;; cargo install svm-rs
;; svm install x.x.x
;; svm use x.x.x
;;
;; ethlint (aka solium)
;; npm install -g solidity-language-server ethlint
;;
;; formatting: solhint (or prettier)
;; npm install -g solhint
;; or
;; npm install -g prettier prettier-plugin-solidity

;; TODO: turn to yasnpippet snippets https://github.com/chevdor/EthereumSoliditySnippets
;; https://github.com/joaotavora/yasnippet

;; Solidity Development Setup

(use-package solidity-mode
  :ensure
  :mode "\\.sol\\'"
  :hook ((solidity-mode . my/solidity-mode-setup)
         (solidity-mode . lsp)
         (lsp-mode . lsp-ui-mode))
  :config
  (setq solidity-comment-style 'slash))

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil) ;; Use flycheck instead of flymake
  :config
  (add-to-list 'exec-path "~/.nvm/versions/node/v23.11.0/bin")
  (setq lsp-enable-snippet t
        lsp-enable-symbol-highlighting t))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t))

(use-package flycheck
  :defer t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package solidity-flycheck
  :ensure
  :after (solidity-mode flycheck)
  :config
  (setq solidity-flycheck-solc-checker-active t
        solidity-flycheck-chaining-error-level t
        solidity-flycheck-use-project t
        solidity-flycheck-solc-addstd-contracts t)
  ;; Adjust these paths to your environment
  (setq solidity-solc-path "/home/filip/.cargo/bin/solc"
        solidity-solium-path "/home/filip/.nvm/versions/node/v23.11.0/bin/solium"))

(use-package company-solidity
  :ensure
  :after (company solidity-mode)
  :config
  (add-to-list 'company-backends 'company-solidity))

(use-package paredit
  :hook (solidity-mode . paredit-mode))

;; (defun my/solidity-format-buffer ()
;;   "Format current Solidity buffer using prettier with project-specific config."
;;   (interactive)
;;   (when (eq major-mode 'solidity-mode)
;;     (let* ((project-root (locate-dominating-file default-directory ".prettierrc.json"))
;;            (prettier-config (if project-root
;;                                 (concat project-root ".prettierrc.json")
;;                               "~/.prettierrc.json"))
;;            (prettier-args `("--plugin=prettier-plugin-solidity"
;;                             "--write" buffer-file-name)))
;;       ;; Remove trailing whitespace first
;;       (delete-trailing-whitespace)

;;       ;; Format with prettier using project config if available
;;       (if (file-exists-p prettier-config)
;;           (setq prettier-args (append prettier-args (list "--config" prettier-config))))

;;       (apply #'call-process "prettier" nil nil nil prettier-args)
;;       (revert-buffer t t t))))

(defun my/solidity-format-buffer ()
  "Format Solidity buffer using Prettier or Solhint with per-project config detection.
If the buffer has unsaved changes, prompt to save before formatting."
  (when (eq major-mode 'solidity-mode)
    ;; Ensure the file is saved so formatting operates on the latest content.
    (when (buffer-modified-p)
      (if (y-or-n-p "Buffer modified. Save before formatting? ")
          (save-buffer)
        (error "Buffer must be saved before formatting")))
    (let* ((file (buffer-file-name))
           (project-root (or (locate-dominating-file file ".prettierrc")
                             (locate-dominating-file file ".prettierrc.json")
                             (locate-dominating-file file ".solhint.json")))
           (default-directory (or project-root default-directory))
           (prettier (executable-find "prettier"))
           (solhint (executable-find "solhint"))
           (orig-point (point))
           (formatted nil))
      (cond
       ;; Try Prettier if a .prettierrc or .prettierrc.json exists in the project root.
       ((and prettier
             (or (file-exists-p (expand-file-name ".prettierrc" default-directory))
                 (file-exists-p (expand-file-name ".prettierrc.json" default-directory))))
        (shell-command
         (format "%s --plugin=prettier-plugin-solidity --write %s"
                 prettier (shell-quote-argument file)))
        (setq formatted t))
       ;; Otherwise, try Solhint if .solhint.json is found.
       ((and solhint
             (file-exists-p (expand-file-name ".solhint.json" default-directory)))
        (shell-command
         (format "%s --fix %s"
                 solhint (shell-quote-argument file)))))
      ;; Revert buffer to pick up the external formatter changes.
      (revert-buffer :ignore-auto :noconfirm)
      (goto-char orig-point))))

;; (defun my/solidity-format-buffer ()
;;   "Format Solidity buffer using Prettier or Solhint, with per-project config detection."
;;   (when (eq major-mode 'solidity-mode)
;;     (let* ((file (buffer-file-name))
;;            (project-root (or (locate-dominating-file file ".prettierrc")
;;                              (locate-dominating-file file ".prettierrc.json")
;;                              (locate-dominating-file file ".solhint.json")))
;;            (default-directory (or project-root default-directory))
;;            (prettier (executable-find "prettier"))
;;            (solhint (executable-find "solhint"))
;;            (formatted nil))
;;       ;; Try prettier with plugin
;;       (when (and prettier
;;                  (or (file-exists-p (expand-file-name ".prettierrc" default-directory))
;;                      (file-exists-p (expand-file-name ".prettierrc.json" default-directory))))
;;         (shell-command
;;          (format "%s --plugin=prettier-plugin-solidity --write %s"
;;                  prettier (shell-quote-argument file)))
;;         (revert-buffer :ignore-auto :confirm)
;;         (setq formatted t))
;;       ;; Fallback: try solhint if .solhint.json is present
;;       (unless formatted
;;         (when (and solhint
;;                    (file-exists-p (expand-file-name ".solhint.json" default-directory)))
;;           (shell-command
;;            (format "%s --fix %s"
;;                    solhint (shell-quote-argument file)))
;;           (revert-buffer :ignore-auto :confirm)
;;           )))))

(defun my/solidity-mode-setup ()
  "Custom configurations for Solidity mode."
  ;; Enable Flycheck only in Solidity buffers
  (flycheck-mode 1)

  ;; Set company backends for local + language symbols
  (set (make-local-variable 'company-backends)
       (append '((company-solidity company-capf company-dabbrev-code))
               company-backends))

  ;; Add formatting on save
  (add-hook 'after-save-hook #'my/solidity-format-buffer nil t)
)

(use-package yasnippet
  :ensure
  :hook (solidity-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure
  :after yasnippet
  :defer t)

;; Optional: Load custom Solidity snippets
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets" ...))

(provide 'init-solidity)

;; fixes the annoying "Error running timer ‘lsp--on-idle’: (error "The connected server(s) does not support method textDocument/inlayHint."
;; (setq lsp-enable-links nil)
(setenv "LSP_USE_PLISTS" "true")
;; (setq lsp-use-plists t)
