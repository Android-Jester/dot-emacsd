;;; keymaps.el --- Keymaps for emacs                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <duah14@outlook.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package evil
  :straight t
  :config
  (setq evil-want-integration t
	    evil-want-C-u-scroll t
	    evil-want-C-i-jump nil 
	    evil-ex-search-vim-style-regexp t
	    evil-ex-visual-char-range t
        evil-mode-line-format 'nil
	    evil-symbol-word-search t
	    evil-default-cursor '+evil-default-cursor-fn
	    evil-normal-state-cursor 'box
	    evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
	    evil-insert-state-cursor 'bar
	    evil-visual-state-cursor 'hollow
	    evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t)
  (setq-default tab-width 4
                indent-tabs-mode nil
		        tab-always-indent t)
  (evil-mode)
  (define-key evil-insert-state-map (kbd "C-`") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "<backtab>") 'tab-to-tab-stop)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal))



(use-package evil-collection
  :straight t
  :after evil
  :init (evil-collection-init))

(use-package evil-commentary
  :straight t
  :after evil
  :init (evil-commentary-mode))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :custom (which-key-idle-delay 0.5))

(use-package undo-fu
  :straight t
  :after evil
  :custom (evil-undo-system 'undo-fu))

(use-package undo-fu-session
  :straight t
  :after (evil undo-fu))

(use-package general
  :straight t
  :after (evil evil-collection evil-commentary)
  :init (general-evil-setup)
  :config (general-create-definer keys/leader
	        :states '(normal visual emacs)
	        :prefix "SPC")

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; KeyMaps
;;; ------------------------------------------------------------------------------------------------------------------------

(keys/leader
  "e"   '(find-file :wk "Find File")
  "d"   '(dired :wk "Find folder")
  "w"   '(evil-window-map :wk "[w]indow"))

(keys/leader
  "b"   '(:wk "Buffers")
  "b b" '(consult-buffer :wk "Browse Buffers")
  "b d" '(kill-buffer :wk "Kill Buffer")
  "b i" '(ibuffer :wk "ibuffer")) 

(defun conf/init ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(keys/leader
  "f" '(:wk "Config Prefix")
  "f e" '(conf/init :wk "Open Init.el"))


(defun conf/open-org-directory ()
  (interactive)
  (dired org-directory))

(keys/leader
  "o" '(:wk "OrgMode Stuffs")
  "o A" '(org-agenda :wk "Org Agenda")
  "o o" '(conf/open-org-directory :wk "Org directory")
  "o f j" '(org-journal-find-file :wk "Find Journal File")
  "o j" '(org-journal-new-date-entry :wk "Org Journal Date Entry")
  "f O" '(( lambda () (interactive) (find-file (expand-file-name "~/Notes"))) :wk "Personal Notes")
  "f P" '(( lambda () (interactive) (find-file (expand-file-name "~/Org"))) :wk "Org Stuffs"))


(keys/leader
  :keymap 'org-journal-mode-map
  "o J" '(org-journal-previous-entry :wk "[O]rg [j]ournal previous entry"))

(keys/leader
  :keymaps 'org-mode-map
  "o" '(:wk "OrgMode Stuffs")
  "o e" '(org-edit-special :wk "Org src-block special edit")
  "o c" '(org-toggle-checkbox :wk "Toggle Checkbox")
  "o l" '(org-insert-link :wk "Insert Link"))

(keys/leader
  :keymap 'projectile-mode-map
  "p" '(:wk "Projects")
  "p a" '(projectile-add-known-project :wk "Add Project")
  "p p" '(projectile-switch-project :wk "Switch/open Project")
  "p R" '(projectile-remove-current-project-from-known-projects :wk "Remove Current Project")
  "p r" '(projectile-remove-known-project :wk "Remove Project")
  "p P"  '(projectile-command-map :wk "projectile Commands"))

(keys/leader
  "h" '(:wk "[H]elpful Commands")
  "h a" '(apropos-command :wk "All Description")
  "h f" '(describe-function :wk "[H]elpful describe [F]unction")
  "h k" '(describe-key :wk "[H]elpful Describe [k]ey")
  "h K" '(describe-keymap :wk "[H]elpful Describe [K]eymap")
  "h v" '(describe-variable :wk "[H]elpful Describe [v]ariable"))

(evil-define-key 'insert 'prog-mode-map (kbd "C-SPC") #'completion-at-point)
(eldoc-add-command #'corfu-insert)
;;(evil-define-key 'insert 'corfu-map (kbd "<tab>") #'corfu-next)
;;(evil-define-key 'insert 'corfu-map (kbd "<backtab>") #'corfu-previous)

(keys/leader
  :keymaps 'eglot-mode-map
  "c" '(:wk "Code")
  "c c" '(compile :wk "Compile")
  "c C" '(recompile :wk "Recompile")
  "c a" '(eglot-code-actions :wk "Code Actions")
  "c d" '(xref-find-definitions :wk "Find Definition")
  "c D" '(eglot-find-typeDefinition :wk "Find Type Definition")
  "c R" '(xref-find-references :wk "Find Implementations")
  "c r" '(eglot-rename :wk "Rename Symbol"))


(keys/leader
  "t" '(:wk "Toggleables")
  "t t" '(vterm-toggle-cd :wk "Toggle Terminal")))


(provide 'keymaps)
;;; keymaps.el ends here
