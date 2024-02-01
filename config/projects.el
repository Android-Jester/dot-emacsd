;;; projects.el --- Project packages for project management  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Duah Kwadwo Adjei

;; Author: Duah Kwadwo Adjei <duah14@outlook.com>
;; Keywords: lisp

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

(use-package projectile
  :straight t
  :custom
  ;; (projectile-completion-system 'vertico)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-indexing-method 'native)
  (projectile-sort-order 'modification-time)
  (projectile-enable-caching t)
  (projectile-auto-discover nil)
  (projectile-require-project-root t)
  :init (projectile-mode)
  :config
  (projectile-register-project-type 'zig '("build.zig.zon")
                                    :project-file "build.zig.zon"
				                    :compile "zig build"
				                    :test "zig build test"
				                    :run "zig build run")
  ;; .NET C# or F# projects
  (projectile-register-project-type 'dotnet #'projectile-dotnet-project-p
                                    :project-file '("?*.csproj" "?*.fsproj")
                                    :compile "dotnet build"
                                    :run "dotnet run"
                                    :test "dotnet test")
  (setq projectile-project-search-path '(
                                         "~/Documents"
                                         "~/Projects"
                                         "~/Org")))


(use-package org-project-capture
  :straight t)

(use-package org-projectile
  :straight t
  :after (org org-project-capture projectile)
  :config
  (setq org-project-capture-default-backend (make-instance 'org-project-capture-projectile-backend)
        org-project-capture-projects-file "~/Project/projects.org"))


(use-package magit
  :straight t)

(use-package magit-todos
  :straight t)

(use-package bufler
  :straight t)
  ;;:config
  ;;(add-hook 'after-init-hook #'bufler-workspace-mode))

(use-package ibuffer-projectile
  :straight t
  :after projectile)

(use-package ibuffer-vc
  :straight t
  :after (projectile ibuffer-projectile))

(provide 'projects)
;;; projects.el ends here
