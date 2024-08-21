(setq read-process-output-max (* 1024 1024)) ;; 1mb (setq gc-cons-threshold 100000000)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Smooth scrolling
(pixel-scroll-precision-mode t)

(setq native-comp-async-report-warnings-errors 'silent)
