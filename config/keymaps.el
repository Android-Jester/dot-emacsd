;;; keymaps.el --- keybinds                          -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author:  <androidjester@JesterTUF>
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


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-SPC"))

;; Evil Mode
(setq evil-want-integration t
	  evil-want-C-u-scroll t
	  evil-want-C-i-jump nil
	  evil-ex-search-vim-style-regexp t
	  evil-ex-visual-char-range t  ; column range for ex commands
	  evil-mode-line-format 'nil
	  evil-symbol-word-search t  ;; more vim-like behavior
	  ;; evil-default-cursor '+evil-default-cursor-fn ;; if the current state is obvious from the cursor's color/shape, then we won't need superfluous indicators to do it instead.
	  evil-normal-state-cursor 'box
	  evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
	  evil-insert-state-cursor 'bar
	  evil-visual-state-cursor 'hollow
	  evil-ex-interactive-search-highlight 'selected-window ;; Only do highlighting in selected window so that Emacs has less work to do highlighting them all.
	  evil-kbd-macro-suppress-motion-error t ;; It's infuriating that innocuous "beginning of line" or "end of line" errors will abort macros, so suppress them:
	  evil-want-keybinding nil
	  ;; Evil collections
	  evil-collection-outline-bind-tab-p nil)

(evil-mode)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(setq-default tab-width 4
			  evil-shift-width tab-width
			  indent-tabs-mode t)

(evil-collection-init)  ;; Evil Collection
(evil-commentary-mode) ;; Evil Commentary


;; general
(general-evil-setup)
(general-create-definer perm/leader
  :states '(normal visual emacs)
  :prefix "SPC")


(which-key-mode)
(setq which-key-idle-delay 1)
(global-undo-tree-mode)


;; -------------------------------------------------------_------------

;; All Keybinds

(defun conf/config-find ()
  (interactive) (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

(perm/leader
  "f i" '(conf/config-find :wk "Config: init.el"))

(perm/leader
  "e" '(find-file :wk "Find File")
  "w" '(evil-window-map :wk "[w]indow"))

;; Browsers
(perm/leader
  "s" '(consult-line :wk "[s]wiper Search"))

;; Helpful
(perm/leader
  "h" '(:wk "[H]elpful Commands")
  "h a" '(apropos-command :wk "All Description")
  "h f" '(describe-function :wk "[H]elpful describe [F]unction")
  "h k" '(describe-key :wk "[H]elpful Describe [k]ey")
  "h K" '(describe-keymap :wk "[H]elpful Describe [K]eymap")
  "h v" '(describe-variable :wk "[H]elpful Describe [v]ariable"))

;; Buffers
(perm/leader
  "b" '(consult-buffer :wk "[B]uffer List"))


(perm/leader
  "t" '(:wk "Togglers")
  "t t" '(treemacs :wk "[T]reemacs Open")
  "t v" '(vterm-toggle-cd :wk "[v]term toggle"))


(defun conf/open-org-directory ()
  (interactive)
  (dired org-directory))

(perm/leader
  "o" '(:wk "OrgMode Stuffs")
  "o A" '(org-agenda :wk "Org Agenda")
  "o o" '(conf/open-org-directory :wk "Org directory")
  "o j" '(org-journal-new-date-entry :wk "Org Journal Date Entry"))


(perm/leader
  :keymaps 'org-mode-map
  "o" '(:wk "OrgMode Stuffs")
  "o e" '(org-edit-special :wk "Org src-block special edit")
  "o c" '(org-toggle-checkbox :wk "Toggle Checkbox")
  "o l" '(org-insert-link :wk "Insert Link"))



;; (evil-define-minor-mode-key 'normal lsp-mode (kbd "SPC l") lsp-command-map)

(perm/leader
  :keymaps 'lsp-mode-map
  "c" '(:wk "Lsp Map")
  "c a" '(lsp-execute-code-action :wk "Code Action")
  "c k" '(compile :wk "Compile Command")
  "c r" '(lsp-rename :wk "Rename Identifier")
  "c d" '(lsp-ui-peek-find-definitions :wk "Go to Definition")
  "c R" '(lsp-ui-peek-find-references :wk "Find References")
  "c w" '(lsp-ui-peek-find-implementation :wk "Find Implementations")
  "c f" '(lsp-format-buffer :wk "Code Formatting")
  "c E" '(lsp-treemacs-errors-list :wk "Error List")
  "c m" '(lsp-ui-imenu :wk "Symbol Menu")
  "c e" '(lsp-treemacs-errors-list :wk "Error List")
  "c S" '(lsp-treemacs-symbols :wk "Code Symbols"))


(evil-global-set-key 'insert (kbd "C-SPC") #'company-complete-common)

(with-eval-after-load 'evil
  (with-eval-after-load 'company
	(setq company-require-match nil)
	(define-key evil-insert-state-map (kbd "C-n") nil)
	(define-key evil-insert-state-map (kbd "C-p") nil)
	(evil-define-key nil company-active-map (kbd "<tab>") #'company-complete-common-or-cycle)
	(evil-define-key nil company-active-map (kbd "<backtab>") #'company-select-previous)))


(provide 'keymaps)
;;; keymaps.el ends here
