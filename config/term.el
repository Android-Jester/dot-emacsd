;;; term.el ---                                      -*- lexical-binding: t; -*-

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
;; (vterm-module-compile)
(evil-set-initial-state 'vterm-mode 'emacs)
		  (add-hook 'vterm-mode-hook (lambda ()
									   (setq-local global-hl-line-mode nil)
									   (setq-local truncate-lines t)))
		  ;; Default value plus F12
		  (setq vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y" "<f12>"))

(provide 'term)
;;; term.el ends here
