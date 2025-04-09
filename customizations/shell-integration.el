;; set shell to be bash (fish confuses ag)
(setq explicit-shell-file-name "/bin/bash")

;; shell scripts indentation
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; get SSH agent in magit, don't ask for passw every time
;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-initialize)
;; (exec-path-from-shell-copy-env "SSH_AGENT_PID")
;; (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-envs '("PATH")))

;; Sets up exec-path-from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  ;; get SSH agent in magit, don't ask for passw every time
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  ;; for deepseek API key from .secrets file
  (exec-path-from-shell-copy-env "DEEPSEEK_API_KEY")

  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs '("PATH")))

  )
