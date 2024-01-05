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
(vertico-mode)
(savehist-mode)
(vertico-posframe-mode)

(setq vertico-multiform-commands
      '((consult-line
         posframe
         (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
         (vertico-posframe-border-width . 10)
         ;; NOTE: This is useful when emacs is used in both in X and
         ;; terminal, for posframe do not work well in terminal, so
         ;; vertico-buffer-mode will be used as fallback at the
         ;; moment.
         (vertico-posframe-fallback-mode . vertico-buffer-mode))
        (t posframe)))
(vertico-multiform-mode 1)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq marginalia-max-relative-age 0)
(setq marginalia-align 'right)
(marginalia-mode)

(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)


(setq completion-styles '(orderless basic)
	  completion-category-overrides '((file (styles basic partial-completion))))

;; (setq ivy-height 15
;; (ivy-mode)
;; 	  ivy-wrap t
;; 	  ivy-fixed-height-minibuffer nil
;; 	  ivy-use-virtual-buffers t
;; 	  ivy-use-selectable-prompt nil
;; 	  ivy-initial-inputs-alist nil
;; 	  ivy-rich-parse-remote-buffer nil
;; 	  swiper-action-recenter t
;; 	  ivy-prescient-enable-filtering nil)
;; (counsel-mode)
;; (amx-mode)
;; (ivy-prescient-mode)
;; (prescient-persist-mode)
;; (nerd-icons-ivy-rich-mode)
;; (ivy-rich-mode)


(provide 'browsers)
;;; browsers.el ends here
