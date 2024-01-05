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



(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install use-package support
(elpaca elpaca-use-package
		;; Enable :elpaca use-package keyword.
		(elpaca-use-package-mode)
		;; Assume :elpaca t unless otherwise specified.
		(setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait);

;;  ------------------------------------------------------------------------------
;;; Keymaps

;;(use-package exum)

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

;;  ------------------------------------------------------------------------------
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
  :elpaca (vertico :files (:defaults "extensions/**")
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
;;; Helpful descripti eqons
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
  (use-package ts-fold :use-package (ts-fold
									 :host github
									 :repo "emacs-tree-sitter/ts-fold"))

  (use-package ts-fold :use-package (ts-fold-indicators
									 :host github
									 :repo "emacs-tree-sitter/ts-fold"))
  (use-package origami)

  ;;  ------------------------------------------------------------------------------
;;; File Browsers
  (use-package dired
	:elpaca nil
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
  (use-package seq)
  (use-package magit)
  ;;  ------------------------------------------------------------------------------
;;; Documenting stuff with org-moe
  (use-package org)
  (use-package org-superstar)
  (use-package org-modern)
  (use-package org-fancy-priorities)

  (use-package org-appear
	:use-package (org-appear :host github :repo "awth13/org-appear"))

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
  (use-package doom-snippets :use-package (doom-snippets :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

  (use-package company
	:after lsp)
  (use-package company-box)
  (use-package apheleia)
  (use-package tree-sitter)
  (use-package tree-sitter-langs)
  (use-package company-irony)
  (use-package company-irony-c-headers)
  (use-package yasnippet-snippets)
  ;;  ------------------------------------------------------------------------------
;;; Programming languages

  ;; emacs lisp
  (use-package elec-pair :use-package nil)
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
  (elpaca-wait)


  ;;  ------------------------------------------------------------------------------
;;; Terminal
  (use-package vterm)
  (use-package vterm-toggle
	:after vterm)

  ;;  ------------------------------------------------------------------------------
;;; Search and browser buffer packages



  (provide 'packages)
;;; packages.el ends here
