;; --- Go development in Docker -----------------------------

(defcustom go-projects-dir "~/CloudStation/GoProjects/"
  "Base directory for all Go projects"
  :type 'string
  :group 'go)

(defcustom go-docker-image "lspcontainers/gopl"
  "Docker image for Go development"
  :type 'string
  :group 'go)

;; (defcustom go-docker-container "golang-lsp-container"
;;   "Docker container name for Go language server"
;;   :type 'string
;;   :group 'go)


;; (defcustom go-docker-volume "go-mod-cache"
;;   "Docker volume for Go module cache"
;;   :type 'string
;;   :group 'go)

;; --- Custom setup for Go projects inside Docker ------------
(defun my-go-docker-setup ()
  "Enable LSP + Flycheck for Go projects under ~/CloudStation/GoProjects"
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name go-projects-dir)
                              (file-name-directory buffer-file-name)))
    (lsp-deferred)
    (company-mode 1)
    (flycheck-mode 1)))

(use-package go-mode
  :hook ((go-mode . my-go-docker-setup)
         (before-save . gofmt-before-save))
  :config
  ;; Key bindings for Docker commands
  (define-key go-mode-map (kbd "C-c C-c") 'go-docker-compile)
  (define-key go-mode-map (kbd "C-c C-t") 'go-docker-test)
  (define-key go-mode-map (kbd "C-c C-r") 'go-docker-run))

(use-package lsp-mode
  :commands lsp
  :hook ((go-mode . lsp-deferred))
  :custom
  (lsp-prefer-flymake nil)
  ;; don’t nag if no client matches (for non-Go modes)
  (lsp-warn-no-matched-clients nil))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; (use-package company
;;   :hook (after-init . global-company-mode))
(use-package company
  :defer t
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; (use-package company-lsp
;;   :after company
;;   :config
;;   (push 'company-lsp company-backends))

(use-package flycheck
  :defer t) ;; don’t enable globally

;; ==================== LSP-DOCKER CONFIGURATION ====================
;; --- README: https://github.com/emacs-lsp/lsp-docker?tab=readme-ov-file#configuration
;; V1
(use-package lsp-docker
  :after lsp-mode
  :config
  ;; Define which LSP clients should be used inside Docker
  (setq lsp-docker-client-packages
        '(lsp-go))  ;; only enable Go; uncomment others if needed

  ;; Define client-specific Docker configurations
  (setq lsp-docker-client-configs
        '((:server-id gopls
           :docker-image-id lspcontainers/gopls:latest
           :docker-container-name gopls-container
           :docker-server-id gopls-docker
           :server-command "gopls")))

  ;; Initialize Docker LSP clients with path mapping
  (lsp-docker-init-clients
   :path-mappings '(("/home/filip/CloudStation/GoProjects" . "/projects"))
   :client-packages lsp-docker-client-packages
   :client-configs lsp-docker-client-configs))

;;; END: LSP DOCKER

;; TODO: docker comands 
