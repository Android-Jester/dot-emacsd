;;; prog-langs.el --- Programming languages and their configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <duah14@outlook.com>
;; Keywords: lisp, c, rust, zig, typescript, dart

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

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Emacs Lisp
;;; ------------------------------------------------------------------------------------------------------------------------

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package elisp-mode
  :straight (:type built-in)
  :config
  (add-hook 'lisp-interactive-mode-hook #'eglot-ensure)
  (add-hook 'emacs-lisp-mode-hook #'auto-insert-mode))

(use-package elec-pair
  :straight nil)



;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Dart
;;; ------------------------------------------------------------------------------------------------------------------------

(use-package dart-mode
  :straight t
  :custom
  (dart-sdk-path (concat (getenv "HOME") "/.local/share/flutter/bin/cache/dart-sdk"))
  (add-to-list'auto-mode-alist '("\\.dart\\'" . dart-mode))
  (add-hook 'dart-mode-hook #'eglot-ensure)
  (add-hook 'dart-mode-hook #'tree-sitter-mode))

(use-package flutter
  :after (dart-mode lsp-dart)
  :straight t
  :custom (flutter-sdk-path (concat (getenv "HOME") "/.local/share/flutter")))

(use-package hover
  :straight t
  :config
  (setq  hover-flutter-sdk-path (concat (getenv "HOME") "/.local/share/flutter")
         hover-command-path (concat (getenv "GOPATH") "/bin/hover") ; remove if `hover` is already in $PATH
         hover-hot-reload-on-save t
         hover-screenshot-path (concat (getenv "HOME") "/Pictures")
         hover-screenshot-prefix "my-prefix-"
         hover-observatory-uri "http://my-custom-host:50300"
         hover-clear-buffer-on-hot-restart t)
  (hover-minor-mode))


;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Go
;;; ------------------------------------------------------------------------------------------------------------------------

(use-package go-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (add-to-list 'auto-mode-alist '("go.mod\\'" . go-mode))
  (add-to-list 'auto-mode-alist '("go.work\\'" . go-mode))
  (add-hook 'go-mode #'eglot-ensure))

(use-package go-eldoc
  :straight t)

(use-package go-guru
  :straight t)

(use-package ob-go
  :straight t)

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; C/C++
;;; ------------------------------------------------------------------------------------------------------------------------

;; (use-package c-mode
;;   :straight nil
;; :config
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-hook 'c-mode-hook 'eglot-ensure)
;;)

;; (use-package c++-mode
;; :straight nil
;; :config
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c++\\'" . c-mode))
(add-hook 'c++-mode-hook 'eglot-ensure)
;;)

;; (use-package ob-c
;;   :straight (:type built-in))

(use-package demangle-mode
  :straight t)

(use-package disaster
  :straight t)

(use-package modern-cpp-font-lock
  :straight t)


(use-package cmake-mode
  :straight t)

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Shell
;;; ------------------------------------------------------------------------------------------------------------------------

;; (use-package sh-mode
;;   :straight (:type built-in)
;; :config
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-hook 'sh-mode #'eglot-ensure)
(add-hook 'sh-mode #'tree-sitter-mode)
;;)

(use-package fish-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
  (add-hook 'fish-mode #'eglot-ensure))

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Lua
;;; ------------------------------------------------------------------------------------------------------------------------
(use-package lua-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  ;;(add-hook 'lua-mode-hook #'lsp)
  (add-hook 'lua-mode-hook #'eglot-ensure)
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Rust
;;; ------------------------------------------------------------------------------------------------------------------------

(use-package rustic
  :straight t)

(use-package ob-rust
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'rust-mode-hook #'eglot-ensure))

;;; ------------------------------------------------------------------------------------------------------------------------
        ;;; Zig
;;; ------------------------------------------------------------------------------------------------------------------------

(use-package zig-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
  (add-to-list 'auto-mode-alist '("\\.zon\\'" . zig-mode))
  (add-hook 'zig-mode-hook #'lsp)
  (add-hook 'zig-mode-hook #'eglot-ensure))

(add-hook 'js-mode-hook 'eglot-ensure)

(use-package web-mode)

(provide 'prog-langs)
;;; prog-langs.el ends here