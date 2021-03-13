;;;;
;; Java
;;;;

;; --- CONF --- ;;

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package hydra)
(use-package company)

;; (use-package lsp-ui)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t
     lsp-ui-sideline-show-symbol t
     lsp-ui-sideline-show-hover t
     lsp-ui-sideline-showcode-actions t
     lsp-ui-sideline-update-mode 'point))

(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
;; (use-package helm-lsp)
;; (use-package helm
;;   :config (helm-mode))
(use-package lsp-treemacs)

;; --- CONFIG EXTRAS --- ;;

;; (setq lsp-java-java-path "/usr/lib/jvm/java-11-oracle/bin/java")
(setq lsp-java-java-path "/usr/bin/java")

;; add lombok support
(setq lombok-version "1.18.16")
;; https://projectlombok.org/
(setq lsp-java-vmargs '("-noverify"
                        "-Xmx1G"
                        "-XX:+UseG1GC"
                        "-XX:+UseStringDeduplication"
                        ;; (format "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)
                        ;; (format "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)
                        "-javaagent:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
                        ;; "-Xbootclasspath/a:/home/filip/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"
                        ))

(add-hook 'java-mode-hook
          (lambda () (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)))

(add-hook 'java-mode-hook #'enable-paredit-mode)

(setq lsp-keep-workspace-alive nil)
