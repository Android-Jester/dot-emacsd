;; Some changes to make it more personalized
(load-theme 'tango-dark)
(setq warning-minimum-level :error
	  ;; frame-title-format '("Jester's Emacs")
	  ring-bell-function 'ignore
	  frame-resize-pixelwise t
	  default-directory "~/"
	  inhibit-startup-message t)
(global-so-long-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;;(tooltop-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'cat-and-mouse)
;; Disable Line Numbers for some major modes
(dolist (mode '(
				treemacs-mode-hook
				eshell-mode-hook
				vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; Folder Clean Up
(delete-selection-mode)

(setq confirm-kill-process nil
	  create-lockfiles nil
	  make-backup-files nil
	  auto-revert-interval 5
	  auto-revert-non-file-buffers t
	  auto-revert-verbose nil)

(global-auto-revert-mode)

(setq native-comp-always-compile t)
(defvar perm/default-font-size 115)

;; Set Main Face Attribute
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height perm/default-font-size)

;; Face for fixed sizes
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height perm/default-font-size)

;; Variable faces
(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height perm/default-font-size)
