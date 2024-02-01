;;; browsers.el --- Browser packages                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <duah14@outlook.com>
;; Keywords: lisp, lisp

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

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  :config
  (setq delete-by-moving-to-trash t
	    auto-save-file-transforms '((".*" (no-littering-expand-var-file-name "auto-save/") t))
        dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        dired-create-destination-dirs 'ask
        image-dired-dir (concat cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-thumb-size 150))

(use-package dired-single
  :straight t
  :commands (dired dired-jump)
  :bind
  ([remap dired-find-file] . dired-single-buffer)
  ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
  ([remap dired-up-directory] . dired-single-up-directory))

(use-package ranger
  :straight t
  :custom
  (ranger-override-dired t)
  (ranger-cleanup-on-disable t)
  (ranger-cleanup-eagerly t)
  (ranger-show-hidden t)
  (ranger-preview-delay 0.040)
  (ranger-preview-file t)
  (ranger-dont-show-binary t))

(use-package no-littering
  :straight t)

(provide 'browsers)
;;; browsers.el ends here
