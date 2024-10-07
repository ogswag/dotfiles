(setq read-process-output-max (* 1024 1024)) ;; 1mb (setq gc-cons-threshold 100000000)

;; Disable splash screen
(setq inhibit-startup-screen t)

(tool-bar-mode -1)    ;; No toolbar
(scroll-bar-mode -1)  ;; No scroll bars
(context-menu-mode 1) ;; Enable right click menus

(setq native-comp-async-report-warnings-errors 'silent)
