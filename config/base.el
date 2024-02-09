;;; base.el --- The basic emacs set of configurations  -*- lexical-binding: t; -*-

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

(use-package emacs
  :straight nil
  :config
  (setq warning-minimum-level :error
	    frame-title-format '("Jester's Emacs")
	    ring-bell-function 'ignore
	    frame-resize-pixelwise t
	    default-directory "~/"
        inhibit-startup-message t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (dolist (mode '(
		          prog-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode))))
  (delete-selection-mode)
  (setq create-lockfiles nil
	    make-backup-files nil
	    native-comp-always-compile t)
  (global-auto-revert-mode)
  (menu-bar--display-line-numbers-mode-relative)
  (defvar conf/default-font-size 115)
  (defvar conf/font "JetBrains Mono")
  
  ;; Set Main Face Attribute
  (set-face-attribute 'default nil :font conf/font :height conf/default-font-size)

  ;; Face for fixed sizes
  (set-face-attribute 'fixed-pitch nil :font conf/font :height conf/default-font-size)

  ;; Variable faces
  (set-face-attribute 'variable-pitch nil :font conf/font :height conf/default-font-size)

  ;; changing some default keybinds
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-unset-key (kbd "C-SPC")))
(provide 'base)
;;; base.el ends here
