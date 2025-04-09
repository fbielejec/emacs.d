;; this ENV var should be exported, ideally in .secrets under .bashrc
;; (setq gptel-api-key "${DEEPSEEK_API_KEY}")

(setq gptel-default-mode "org-mode")

;; OPTIONAL configuration to set DeepSeek as the  default backend for gptel,
(setq gptel-model   'deepseek-reasoner
      gptel-backend (gptel-make-deepseek "DeepSeek"
                      :stream t
                      :key (getenv "DEEPSEEK_API_KEY") ;;"your-api-key" ;; can be a function that returns the key
                      ))
