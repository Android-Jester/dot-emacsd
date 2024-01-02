;;; interface.el --- This is for UI stuffs           -*- lexical-binding: t; -*-

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

(load-theme 'doom-one t)
(doom-modeline-mode)
(setq doom-modeline-height 40)
(add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
(add-hook 'elpaca-after-init-hook #'dashboard-initalize)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Today is going to be better for me to get better"
	  dashboard-starteup-banner (concat user-emacs-directory "/assets/logo.png")
	  dashboard-center-content t
	  dashboard-items '((recents  . 5)
						(bookmarks . 5)
						(projects . 5)
						(agenda . 5)
						(registers . 5))
	  dashboard-icon--type 'all-the-icons
	  dashboard-set-heading-icons t
	  dashboard-set-file-icons t
	  dashboard-footer-messages '("Welp, lets get to work")
	  dashboard-footer-icon (all-the-icons-octicon "dashboard"
												   :height 1.1
												   :v-adjust -0.05
												   :face 'font-lock-keyword-face)
	  dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
	  dashboard-week-agenda t)

(provide 'interface)
;;; interface.el ends here
