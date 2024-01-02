;;; projects.el ---                                  -*- lexical-binding: t; -*-

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

(projectile-mode)
(provide 'projects)
;;; projects.el ends here

(when (file-directory-p "~/Documents")
  (setq projectile-project-search-path '("~/Documents")))

(setq projectile-switch-project-action #'projectile-dired
	  projectile-auto-discover nil)
(counsel-projectile-mode)
