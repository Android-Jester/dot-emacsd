;;; init.el --- My Basic Emacs config                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <androidjester@JesterLatitude>
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

(setq user-full-name "Duah Kwadwo Adjei"
      user-mail-address "duah14@outlook.com"
      user-login-name "AndroidJester"
      auth-sources '("~/.authinfo.gpg")
      cache-dir (concat (getenv "HOME") "/.cache/emacs")
      config-dir (expand-file-name "config/" user-emacs-directory))

(defun conf/load (file)
  (load (expand-file-name file config-dir)))

;; (add-to-list 'load-path (expand-file-name "/config" user-emacs-directory))

(conf/load "packages")
(conf/load "base")
(conf/load "ui")
(conf/load "completers")
(conf/load "browsers")
(conf/load "productivity")
(conf/load "documentation")
(conf/load "projects")
(conf/load "dev")
(conf/load "prog-langs")
(conf/load "keymaps")
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
