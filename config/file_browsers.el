;;; file_browsers.el ---                             -*- lexical-binding: t; -*-

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
(setq delete-by-moving-to-trash t
	  ranger-cleanup-on-disable t
	  ranger-cleanup-eagerly t
	  ranger-show-hidden t
	  ranger-preview-delay 0.040
	  ranger-preview-file t
	  ranger-dont-show-binary t
	  auto-save-file-transforms '((".*" (no-littering-expand-var-file-name "auto-save/") t)))

(eval-after-load "dired"
  #'(lambda ()
	  (put 'dired-find-alternative-file 'disabled nil)
	  (define-key dired-mode-map (kbd "RET") #'dired-find-alternative-file)))

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "l" 'dired-single-buffer)

(diredfl-global-mode)
;; (ranger-override-dired-mode t)

;; (add-hook dired-mode-hook #'all-the-icons-dired-mode)
;; (add-hook dired-mode-hook #'ranger-mode)
(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)

(setq treemacs-indentation 4
	  treemacs-position 'right
	  treemacs-width-increment 3)


(provide 'file_browsers)
;;; file_browsers.el ends here
