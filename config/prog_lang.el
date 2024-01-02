;;; prog_lang.el --- programming languages           -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author:  <androidjester@JesterTUF>
;; Keywords: c, lisp

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


;; emacs lisp
(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
(add-hook 'emacs-lisp-mode-hook #'elisp-def-mode)
(add-hook 'emacs-lisp-mode-hook #'lsp)
(add-hook 'lisp-interactive-mode-hook #'lsp)
(add-hook 'lisp-interactive-mode-hook #'tree-sitter-mode)
(add-hook 'emacs-lisp-mode-hook #'tree-sitter-mode)


;; dart
(setq hdart-sdk-path (concat (getenv "HOME") "/.local/share/flutter/bin/cache/dart-sdk")
	  flutter-sdk-path (concat (getenv "HOME") "/.local/share/flutter")
	  hover-flutter-sdk-path (concat (getenv "HOME") "/.local/share/flutter")
	  hover-command-path (concat (getenv "GOPATH") "/bin/hover") ; remove if `hover` is already in $PATH
	  hover-hot-reload-on-save t
	  hover-screenshot-path (concat (getenv "HOME") "/Pictures")
	  hover-screenshot-prefix "my-prefix-"
	  hover-observatory-uri "http://my-custom-host:50300"
	  hover-clear-buffer-on-hot-restart t)

(hover-minor-mode)

(add-to-list'auto-mode-alist '("\\.dart\\'" . dart-mode))
(add-hook 'dart-mode-hook #'lsp)
(add-hook 'dart-mode-hook #'tree-sitter-mode)

;; C/C++
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c++\\'" . c++-mode))
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook #'tree-sitter-mode)
(add-hook 'c++-mode-hook #'tree-sitter-mode)

;; shell
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-hook 'sh-mode #'lsp)
(add-hook 'fish-mode #'lsp)
(add-hook 'sh-mode #'tree-sitter-mode)
(add-hook 'fish-mode #'tree-sitter-mode)

;; Lua
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(add-hook 'lua-mode-hook #'lsp)
(add-hook 'lua-mode-hook #'tree-sitter-mode)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'tree-sitter-mode)

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.zon\\'" . zig-mode))

(add-hook 'zig-mode-hook #'lsp)
(add-hook 'zig-mode-hook #'tree-sitter-mode)



(provide 'prog_lang)
;;; prog_lang.el ends here
