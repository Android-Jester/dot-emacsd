;;; browsers.el ---                                  -*- lexical-binding: t; -*-

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


(ivy-mode)

(setq ivy-height 15
	  ivy-wrap t
	  ivy-fixed-height-minibuffer nil
	  ivy-use-virtual-buffers t
	  ivy-use-selectable-prompt nil
	  ivy-initial-inputs-alist nil
	  ivy-rich-parse-remote-buffer nil
	  swiper-action-recenter t
	  ivy-prescient-enable-filtering nil)
(counsel-mode)
(amx-mode)
(ivy-prescient-mode)
(prescient-persist-mode)
(nerd-icons-ivy-rich-mode)
(ivy-rich-mode)


(provide 'browsers)
;;; browsers.el ends here
