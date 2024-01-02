;;; orgdoc.el --- Org mode stuffs                    -*- lexical-binding: t; -*-

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

(defvar org-directories (concat (getenv "HOME") "/Documents/Notes"))


(defun perm/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
						  '(("^ *\\([-]\\) "
							 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
				  (org-level-2 . 1.25)
				  (org-level-3 . 1.05)
				  (org-level-4 . 1.0)
				  (org-level-5 . 1.1)
				  (org-level-6 . 1.1)
				  (org-level-7 . 1.1)
				  (org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face)))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun perm/org-mode-setup ()
  "Startup for orgmode"
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (perm/org-font-setup))

(defun perm/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
					  (expand-file-name user-emacs-directory))
	;; Dynamic scoping to the rescue
	(let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle))))


(add-hook 'org-mode-hook #'perm/org-mode-setup)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(add-hook 'org-mode-hook #'org-fancy-priorities-mode)
(add-hook 'org-mode-hook #'org-appear-mode)
(add-hook 'org-mode-hook #'toc-org-mode)
(add-hook 'org-mode-hook (lambda () (evil-org-mode)))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'perm/org-babel-tangle-config)))
(add-hook 'markdown-mode-hook #'toc-org-mode)

(org-superstar-configure-like-org-bullets)
(setq org-ellipsis "..▾"
	  org-src-window-setup 'current-window
	  org-adapt-indentation nil
	  org-directory org-directories
	  org-journal-dir (concat org-directory "/Journal")
	  org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")
	  org-src-preserve-indentation t
	  org-edit-src-content-indentation t)


(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq-default org-startup-indented t
			  org-pretty-entities t
			  org-use-sub-superscripts "{}"
			  org-hide-emphasis-markers t
			  org-startup-with-inline-images t
			  org-image-actual-width '(300))
;; Org Agenda
(defvar org-agenda-dir (concat org-directories "/Personal/Agenda"))
(setq org-habit-graph-column 60
	  org-agenda-start-with-log-mode t
	  org-log-done 'time
	  org-log-into-drawer t
	  org-agenda-files '((concat org-agenda-dir "Tasks.org")
						 (concat org-agenda-dir "Habits.org")
						 (concat org-agenda-dir "Events.org"))
	  org-agenda-custom-commands
	  '(("d" "Dashboard"
		 ((agenda "" ((org-deadline-warning-days 7)))
		  (todo "NEXT"
				((org-agenda-overriding-header "Next Tasks")))
		  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

		("n" "Next Tasks"
		 ((todo "NEXT"
				((org-agenda-overriding-header "Next Tasks")))))

		("W" "Work Tasks" tags-todo "+work-email")

		;; Low-effort next actions
		("e" tags-TODO task description "+TODO=\"NEXT\"+Effort<15&+Effort>0"
		 ((org-agenda-overriding-header "Low Effort Tasks")
		  (org-agenda-max-todos 20)
		  (org-agenda-files org-agenda-files)))

		("w" "Workflow Status"
		 ((todo "WAIT"
				((org-agenda-overriding-header "Waiting on External")
				 (org-agenda-files org-agenda-files)))
		  (todo "REVIEW"
				((org-agenda-overriding-header "In Review")
				 (org-agenda-files org-agenda-files)))
		  (todo "PLAN"
				((org-agenda-overriding-header "In Planning")
				 (org-agenda-todo-list-sublevels nil)
				 (org-agenda-files org-agenda-files)))
		  (todo "BACKLOG"
				((org-agenda-overriding-header "Project Backlog")
				 (org-agenda-todo-list-sublevels nil)
				 (org-agenda-files org-agenda-files)))
		  (todo "READY"
				((org-agenda-overriding-header "Ready for Work")
				 (org-agenda-files org-agenda-files)))
		  (todo "ACTIVE"
				((org-agenda-overriding-header "Active Projects")
				 (org-agenda-files org-agenda-files)))
		  (todo "COMPLETED"
				((org-agenda-overriding-header "Completed Projects")
				 (org-agenda-files org-agenda-files)))
		  (todo "CANC"
				((org-agenda-overriding-header "Cancelled Projects")
				 (org-agenda-files org-agenda-files))))))

	  org-todo-keywords ' ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
						   (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))
	  org-refile-targets '(("Archive.org" :maxlevel . 1) ("Tasks.org" :maxlevel . 1))
	  org-capture-templates `(("t" "Tasks / Projects")
							  ("tt" "Task" entry (file+olp (concat org-agenda-dir "Tasks.org") "Inbox") "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
							  ("j" "Journal Entries")
							  ("jj" "Journal" entry
							   (file+olp+datetree (concat org-agenda-dir "Journal.org"))
							   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
							   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
							   :clock-in :clock-resume
							   :empty-lines 1)

							  ("w" "Workflows")
							  ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
							   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
							  ("m" "Metrics Capture")
							  ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
							   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '(
														   (emacs-lisp . t)))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))


(defvar nb/current-line '(0 . 0)
		  "(start . end) of current line in current buffer")
		(make-variable-buffer-local 'nb/current-line)

		(defun nb/unhide-current-line (limit)
		  "Font-lock function"
		  (let ((start (max (point) (car nb/current-line)))
				(end (min limit (cdr nb/current-line))))
			(when (< start end)
			  (remove-text-properties start end
									  '(invisible t display "" composition ""))
			  (goto-char limit)
			  )))

		(defun nb/refontify-on-linemove ()
		  "Post-command-hook"
		  (let* ((start (line-beginning-position))
				 (end (line-beginning-position 2))
				 (needs-update (not (equal start (car nb/current-line)))))
			(setq nb/current-line (cons start end))
			(when needs-update
			  (font-lock-fontify-block 3))))

		(defun nb/markdown-unhighlight ()
		  "Enable markdown concealling"
		  (interactive)
		  (markdown-toggle-markup-hiding 'toggle)
		  (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
		  (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))

		(add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)
		(add-hook 'markdown-mode-hook #'tree-sitter-mode)


(provide 'orgdoc)
;;; orgdoc.el ends here
