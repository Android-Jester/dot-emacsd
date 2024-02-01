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
  (corfu-auto t)
  (corfu-cycle t)
  :init (global-corfu-mode))


(use-package cape
  :straight t
  :after corfu)

(provide 'dev)
;;; dev.el ends here
