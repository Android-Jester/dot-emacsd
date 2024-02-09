;;; dev.el --- Anything regarding development        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <duah14@outlook.com>
;; Keywords: lisp

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

(use-package vterm
  :straight t
  :config (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil
                                                            truncate-lines t))))

(use-package vterm-toggle
  :after vterm)

(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package eglot
  :straight (:type built-in))

(use-package rainbow-delimiters
  :straight t
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package yasnippet
  :straight t
  :init (yas-global-mode)
  :config
  ;;;(setq yas-snippet-dirs '((expand-file-name "templates" user-emacs-directory)))
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(use-package apheleia
  :straight t
  :init (apheleia-global-mode 1))


(use-package corfu
  :straight t
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  :init (global-corfu-mode))


(use-package cape
  :straight t
  :after corfu)



;; (use-package corfu-candidate-overlay
;;   :straight (:type git
;;                    :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
;;                    :files (:defaults "*.el"))
;;   :after corfu
;;   :config
;;   ;; enable corfu-candidate-overlay mode globally
;;   ;; this relies on having corfu-auto set to nil
;;   (corfu-candidate-overlay-mode +1))
;; bind Ctrl + TAB to trigger the completion popup of corfu
;; (global-set-key (kbd "C-<tab>") 'completion-at-point)
;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;; (keybing <iso-lefttab> may not work for your keyboard model)
;; (global-set-key (kbd "C-<iso-lefttab>") 'corfu-candidate-overlay-complete-at-point))


(provide 'dev)
;;; dev.el ends here
