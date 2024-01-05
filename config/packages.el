t;;; packages.el --- The list of packages for lazy loading  -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author:  <androidjester@JesterTUF>
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



(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("melpa-stable" . "https://stable.melpa.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
						 ("elpa" . "https://elpa.gnu.org/packages/")))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
;;  ------------------------------------------------------------------------------
;;; Keymaps

(use-package evil
  :demand t)

(use-package which-key  
  :demand t)

(use-package general
  :after evil
  :demand t)

(use-package evil-collection
  :after evil)

(use-package evil-commentary
  :after evil)

(use-package undo-tree)

;;; UI elements
(use-package doom-themes)
(use-package doom-modeline)
(use-package all-the-icons)
(use-package evil-anzu)
(use-package dashboard)

;;  ------------------------------------------------------------------------------
;;; Search and browser buffer packages
;; (use-package ivy)
;; (use-package counsel)
;; (use-package amx)

;; (use-package ivy-rich
;;   :after (ivy counsel))

;; (use-package ivy-prescient)

;; (use-package nerd-icons-ivy-rich
;;   :after (ivy-rich ivy counsel))
(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  :straight (vertico :files (:defaults "extensions/**")
					 :includes (vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                )))
(use-package vertico-prescient)
(use-package vertico-posframe)
(use-package consult)
(use-package marginalia
  :after vertico)
(use-package orderless)
(use-package all-the-icons-completion)

;;  ------------------------------------------------------------------------------
;;; Helpful descriptions
(use-package helpful)
;; :custom
;; (counsel-describe-function #'helpful-callable)
;; (counsel-describe-function #'helpful-variable)
;; :bind
;; ([remap describe-function] . counsel-describe-function)
;; ([remap describe-command] . helpful-command)
;; ([remap describe-variable] . counsel-describe-variable)
;; ([remap describe-key] . helpful-key))

;;  ------------------------------------------------------------------------------
;;; Folding
(use-package ts-fold :straight (ts-fold
 								:host github
 								:repo "emacs-tree-sitter/ts-fold"))

(use-package ts-fold :straight (ts-fold-indicators
 								:host github
 								:repo "emacs-tree-sitter/ts-fold"))
(use-package origami)

;;  ------------------------------------------------------------------------------
;;; File Browsers
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump)
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
  ([remap dired-up-directory] . dired-single-up-directory))

(use-package all-the-icons-dired)
(use-package fd-dired)
(use-package dired-git-info)
(use-package diredfl)
(use-package ranger)
(use-package treemacs)
(use-package lsp-treemacs)
(use-package treemacs-icons-dired)
(use-package no-littering)

;;  ------------------------------------------------------------------------------
;;; buffers
(use-package ibuffer-projectile)
(use-package ibuffer-vc)

;;  ------------------------------------------------------------------------------
;;; Project handling
(use-package projectile
  :custom ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :after (counsel projectile))

(use-package treemacs-projectile
  :after (treemacs projectile))
;;(use-package seq)
(use-package magit)


;;  ------------------------------------------------------------------------------
;;; Documenting stuff with org-moe
(use-package org)
(use-package org-superstar)
(use-package org-modern)
(use-package org-fancy-priorities)

(use-package org-appear
  :straight (org-appear :host github :repo "awth13/org-appear"))

(use-package evil-org)
(use-package toc-org)
(use-package org-journal)
;; (use-package org-roam)
(use-package company-org-block
  :after (org company))


(use-package markdown-mode)

;;  ------------------------------------------------------------------------------
;;; Some Programming packages

(use-package rainbow-mode)
(use-package rainbow-delimiters)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package flycheck)
(use-package flycheck-package)
(use-package yasnippet)
;; (use-package doom-snippets :elpaca (doom-snippets :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

(use-package company
  :after lsp)
(use-package company-box)
(use-package apheleia)
(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package company-irony)
(use-package company-irony-c-headers)
(use-package yasnippet-snippets)
(use-package company-quickhelp)
;;  ------------------------------------------------------------------------------
;;; Programming languages

;; emacs lisp
(use-package elec-pair :straight nil)
(use-package highlight-quoted)
(use-package elisp-def)

;; dart
(use-package dart-mode)
(use-package lsp-dart)
(use-package flutter)
(use-package hover)

;; C/C++

(use-package cmake-mode)

;; shell
(use-package company-shell)
(use-package fish-mode)
;; (use-package sh-mode)

;; lua
(use-package lua-mode)

;; Rust
(use-package rustic)

;; Zig
(use-package zig-mode)



;;  ------------------------------------------------------------------------------
;;; Terminal
(use-package vterm)

(use-package vterm-toggle
  :after vterm)

;;  ------------------------------------------------------------------------------
;;; Search and browser buffer packages


(provide 'packages)
;;; packages.el ends here
