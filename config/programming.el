;;; programming.el ---                               -*- lexical-binding: t; -*-

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


(defun +company-box-icons--elisp-fn (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
	(let ((sym (intern candidate)))
	  (cond ((fboundp sym)  'ElispFunction)
			((boundp sym)   'ElispVariable)
			((featurep sym) 'ElispFeature)
			((facep sym)    'ElispFace)))))




(setq lsp-auto-guess-root t
	  lsp-diagnostic-provider :flycheck
	  lsp-enable-snippet t
	  lsp-flycheck-live-reporting t
	  lsp-log-io nil
	  lsp-progress-via-spinner nil
	  lsp-signature-auto-activate nil
	  lsp-headline-breadcrumb-segments '(symbols)
	  lsp-modeline-code-actions-segments '(icon)
	  lsp-ui-sideline-show-hover t
	  lsp-ui-sideline-show-diagnostics t
	  lsp-ui-doc-show-with-cursor t
	  lsp-ui-doc-delay 1
	  yas-snippet-dirs (concat user-emacs-directory "/snippets")
	  lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil
	  company-minimum-prefix-length 2
	  company-tooltip-limit 14
	  company-idle-delay 1
	  company-echo-delay 1
	  company-tooltip-align-annotations t
	  company-tooltip-idle-delay nil
	  company-auto-complete nil
	  company-require-match 'never
	  company-global-modes '(not erc-mode
								 circe-mode
								 message-mode
								 help-mode
								 gud-mode
								 vterm-mode)
	  company-frontends '(company-preview-frontend
						  company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
						  company-echo-metadata-frontend)  ; show selected candidate docs in echo area
	  company-backends '(company-capf
						 company-keywords
						 company-yasnippet
						 company-dabbrev
						 company-abbrev
						 company-dabbrev-code
						 company-semantic
						 company-files
						 company-etags)
	  ;;company-elisp
	  ;;company-clang
	  ;;company-irony-c-headers
	  ;;company-irony
	  ;; company-cmake
	  ;;company-ispell)
	  ;; These auto-complete the current selection when
	  ;; `company-auto-commit-chars' is typed. This is too magical. We
	  ;; already have the much more explicit RET and TAB.
	  company-insertion-on-trigger nil

	  ;; Only search the current buffer for `company-dabbrev' (a backend that
	  ;; suggests text your open buffers). This prevents Company from causing
	  ;; lag once you have a lot of buffers open.
	  company-dabbrev-other-buffers nil
	  company-tooltip-align-annotations t
	  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
	  ;; domain-specific words with particular casing.
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil
	  company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.1)))

(require 'all-the-icons)
(setf (alist-get 'min-height company-box-frame-parameters) 6)
(setq company-box-show-single-candidate t
	  company-box-backends-colors t
	  company-box-tooltip-limit 50
	  x-gtk-resize-child-frames 'resize-mode
	  company-box-icons-alist 'company-box-icons-all-the-icons
	  company-box-backends-colors nil

	  ;; These are the Doom Emacs defaults
	  company-box-icons-all-the-icons `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
										(Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
										(Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
										(Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
										(Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
										(Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
										(Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
										(Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
										(Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
										(Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
										(Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
										(Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
										(Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
										(Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
										(Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
										(Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
										(Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
										(File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
										(Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
										(Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
										(EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
										(Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
										(Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
										(Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
										(Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
										(TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
										(Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))



(lsp-enable-which-key-integration t)

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(global-flycheck-mode)

(eval-after-load 'flycheck
  '(flycheck-package-setup))

(defadvice! +company-box-detect-deleted-frame-a (frame)
  :filter-return #'company-box--get-frame
  (if (frame-live-p frame) frame))

(defadvice! +company-box-detect-deleted-doc-frame-a (_selection frame)
  :before #'company-box-doc
  (and company-box-doc-enable
	   (frame-local-q company-box-doc-frame frame)
	   (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
	   (frame-local-setq company-box-doc-frame nil frame)))

(apheleia-global-mode)
(global-company-mode)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; (add-hook #'prog-mode-hook #'lsp-mode)
(add-hook 'prog-mode-hook #'lsp-mode)
(add-hook 'lsp-mode-hook (lambda () (lsp-headerline-breadcrumb-mode)))
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
(add-hook 'company-yasnippet #'(require 'yasnippet))
(add-hook 'company-mode-hook #'evil-normalize-keymaps)
(add-hook 'elpaca-after-init-hook #'global-company-mode)



(provide 'programming)
;;; programming.el ends here
