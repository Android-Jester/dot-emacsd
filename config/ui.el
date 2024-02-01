;;; ui.el --- Some UI packages just for the road     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <duah14@outlook.com>
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

(use-package doom-themes
  :straight t
  :init (load-theme 'doom-one t))

(use-package solaire-mode
  :straight t
  :after doom-themes
  :init (solaire-global-mode))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode)
  :config
  (display-battery-mode)
  (setq doom-modeline-height 35
	doom-modeline-total-line-number t
	display-time-default-load-average nil
	display-time-24hr-format t)
  (display-time-mode t))

(use-package evil-anzu
  :straight t
  :init (global-anzu-mode))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

(provide 'ui)
;;; ui.el ends here
