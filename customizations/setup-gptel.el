;; this ENV var should be exported, ideally in .secretes under .bashrc
(setq gptel-api-key (getenv "OPENAI_API_PROJECT_KEY"))

(setq gptel-default-mode "org-mode")

;; (gptel-make-deepseek "DeepSeek"       ;Any name you want
;;   :stream t                           ;for streaming responses
;;   :key (getenv "DEEPSEEK_API_KEY")    ;can be a function that returns the key
;;   )
