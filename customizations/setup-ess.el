;; TODO : reuse https://github.com/sejdemyr/.emacs.d/blob/master/init.el

;; disable '_' shortcut
;; (ess-toggle-underscore nil)

(use-package ess-site
  :load-path "ess/lisp/"
  :mode ("\\.R\\'" . R-mode)
  :config
  (validate-setq
   ring-bell-function #'ignore
   ess-ask-for-ess-directory nil
   inferior-R-program-name "/usr/local/bin/R"
   ess-local-process-name "R"
   ansi-color-for-comint-mode 'filter
   comint-scroll-to-bottom-on-input t
   comint-scroll-to-bottom-on-output t
   comint-move-point-for-output t
   ess-default-style 'RStudio)         ; rstudio indentation style

  ;; set assignment operator
  (setq ess-S-assign-key (kbd "s-n"))
  (ess-toggle-S-assign-key t)

  ;; disable '_' shortcut
  (ess-toggle-underscore nil)

  ;; automatically complete parentheses etc
  ;; (add-hook 'ess-mode-hook #'electric-pair-mode)

  ;; set piping operator key binding
  ;; http://emacs.stackexchange.com/questions/8041/how-to-implement-the-piping-operator-in-ess-mode
  ;; (defun then_R_operator ()
  ;;   "R - %>% operator or 'then' pipe operator"
  ;;   (interactive)
  ;;   (just-one-space 1)
  ;;   (insert "%>%")
  ;;   (just-one-space 1))

  ;; (define-key ess-mode-map (kbd "s-N") 'then_R_operator)

  ;; (define-key inferior-ess-mode-map (kbd "s-N") 'then_R_operator)

  ;; key binding to evaluate current line or marked region
  ;; (defun my-ess-eval ()
  ;;   (interactive)
  ;;   (if (and transient-mark-mode mark-active)
  ;;       (call-interactively 'ess-eval-region)
  ;;     (call-interactively 'ess-eval-line)))

  ;; (add-hook 'ess-mode-hook
  ;;           '(lambda()
  ;;              (local-set-key (kbd "s-m") 'my-ess-eval)))

  ;; key binding to evaluate entire region (whether marked or not)
  ;; (defun my-ess-eval2 ()
  ;;   (interactive)
  ;;   (call-interactively 'ess-eval-region-or-function-or-paragraph-and-step))

  ;; (add-hook 'ess-mode-hook
  ;;           '(lambda()
  ;;              (local-set-key (kbd "s-M") 'my-ess-eval2)))

  ;; key binding to load_all() for R devlopment
  ;; (defun my-ess-eval3 ()
  ;;   (interactive)
  ;;   (call-interactively 'ess-r-devtools-load-package))

  ;; (add-hook 'ess-mode-hook
  ;;           '(lambda()
  ;;              (local-set-key (kbd "s-B") 'my-ess-eval3)))

  )
