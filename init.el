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

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'packages)
(require 'base)
(require 'ui)
(require 'completers)
(require 'browsers)
(require 'productivity)
(require 'documentation)
(require 'projects)
(require 'dev)
(require 'prog-langs)
(require 'keymaps)

(provide 'init)
;;; init.el ends here
