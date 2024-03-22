(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(load-theme 'doom-one t)
(display-time-mode)
(display-battery-mode)
(global-anzu-mode)
(doom-modeline-mode)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-display-line-numbers-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
		vterm-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-frame-font "JetBrains Mono-10" nil t)

(hl-line-mode 1)
(electric-pair-mode 1)

;; completions
(vertico-mode)
(vertico-prescient-mode)
(global-corfu-mode)
(corfu-prescient-mode)
(marginalia-mode)

(setq evil-want-keybinding nil)
(evil-mode)
(evil-collection-init)
(corfu-popupinfo-mode 1)

;; programming
(rainbow-delimiters-mode)
(apheleia-global-mode)
(require 'nerd-icons-corfu-formatter)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work 
 '(corfu-auto t)
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-selected-packages
   '(nerd-icons-corfu cape apheleia evil-anzu vterm rainbow-delimiters evil-commentary evil-collection helpful consult marginalia corfu corfu-prescient vertico-prescient prescient vertico doom-modeline doom-themes evil-visual-mark-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
