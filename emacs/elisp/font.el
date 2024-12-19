(cond
 ((eq system-type 'windows-nt)
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas 12" t t)))
 ((eq system-type 'darwin) ; macOS
  (when (member "Menlo" (font-family-list))
    (set-frame-font "Menlo 12" t t)
    (set-face-attribute 'fixed-pitch nil :family "Ubuntu Mono")
    (set-face-attribute 'variable-pitch nil :family "Arial")))
 ((eq system-type 'gnu/linux)
  (when (member "Ubuntu Mono" (font-family-list))
    (set-frame-font "Ubuntu Mono 14" t t))))
