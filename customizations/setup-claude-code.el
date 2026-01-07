;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Aider / Aidermacs

(use-package aidermacs
  :ensure t
  :commands (aider-chat aider-open-file aider-rewrite)
  :bind (("C-c a m" . aidermacs-transient-menu))
  :custom
  ;; See the Configuration section below
  (aidermacs-default-chat-mode 'code)
  ;; (aidermacs-default-model "openrouter/anthropic/claude-3.7-sonnet")
  (aidermacs-default-model "sonnet")
  :config
  ;; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
  ;; (setenv "ANTHROPIC_API_KEY" "sk-...")

  ;; (setq aider-model "gpt-4.1"
  ;;       aider-editor-model "gpt-4.1-mini"
  ;;       aider-architect t
  ;;       aider-auto-commit nil
  ;;       aider-include-file-tree t)

  (defun aider-rust-fix ()
    "Run cargo check and send error output to Aider."
    (interactive)
    (let ((buf (get-buffer-create "*Cargo Check*")))
      (with-current-buffer buf (erase-buffer))
      (call-process "cargo" nil buf nil "check")

      (let ((errors (with-current-buffer buf (buffer-string))))
        (aider-chat errors))))

  ;; Keybindings
  (define-key global-map (kbd "C-c a c") #'aider-chat)
  (define-key global-map (kbd "C-c a o") #'aider-open-file)
  (define-key global-map (kbd "C-c a r") #'aider-rewrite)
  (define-key global-map (kbd "C-c a f") #'aider-rust-fix)

  )

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; Claude Code

;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
(use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
