(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "local/doom-snippets" user-emacs-directory))
;;; Appearance
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "JetBrains Mono-11")
   ((eq system-type 'gnu/linux) "JetBrainsMono Nerd Font-11")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))



(custom-set-variables '(display-line-numbers-type (quote relative)))
(solaire-global-mode)
(load-theme 'doom-one t)

(setq doom-modeline-height 30
      doom-modeline-height 35
      doom-modeline-total-line-number t
      display-time-default-load-average nil
      display-time-24hr-format t)

(display-time-mode)
(display-battery-mode)
(global-anzu-mode)
(doom-modeline-mode)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-display-line-numbers-mode)
(global-auto-revert-mode)


(electric-pair-mode 1)

;; completions
(vertico-mode)
(vertico-prescient-mode)
(marginalia-mode)
(require 'doom-snippets)
(yas-global-mode)
(custom-set-variables
 '(corfu-auto t)
 '(corfu-cycle t))
(add-to-list 'completion-at-point-functions #'yasnippet-capf)
(add-to-list 'completion-at-point-functions #'cape-file)
(eldoc-add-command #'corfu-insert)
(global-corfu-mode)
(corfu-prescient-mode)
(corfu-popupinfo-mode 1)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; file handling
(custom-set-variables '(dired-listing-switches "-agho --group-directories-first")
		      '(ranger-override-dired t)
		      '(ranger-cleanup-eagerly t)
		      '(ranger-cleanup-on-disable t)
		      '(ranger-show-hidden t))




;; KeyBinds
(setq evil-want-keybinding nil)
(evil-mode)
(evil-collection-init)
(evil-commentary-mode 1)
(which-key-mode)
(general-evil-setup)
(evil-set-initial-state 'messages-buffer-mode 'normal)


;; programming
(global-wakatime-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)
(apheleia-global-mode)
(global-hl-line-mode)
(global-flycheck-mode)

(require 'treesit-auto)
(require 'tree-sitter)

(custom-set-variables '(treesit-auto-install 'prompt))
(global-treesit-auto-mode)
(dashboard-setup-startup-hook)

;; Set all the tree-sitter modes
(dolist (mode '(dart-mode-hook
		go-mode-hook
		rust-mode-hook
		zig-mode-hook
		lua-mode-hook
		sh-mode-hook
		fish-mode-hook)) (add-hook 'mode #'tree-sitter-mode))

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.zon\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c++\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'auto-mode-alist '("go.mod\\'" . go-mode))
(add-to-list 'auto-mode-alist '("go.work\\'" . go-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))


;; Org mode
(ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                     "\\\\" "://"))
(global-ligature-mode)


(defun conf/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face))))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-ellipsis "{...}▾"
      org-agenda-start-with-log-mode t
      org-habit-graph-column 60)
(advice-add 'org-refile :after 'org-save-all-org-buffers)
(add-hook 'org-mode-hook #'toc-org-mode)



(define-key evil-insert-state-map (kbd "C-`") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
(define-key evil-insert-state-map (kbd "<backtab>") 'tab-to-tab-stop)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(general-create-definer keys/leader
  :states '(normal visual emacs)
  :prefix "SPC")


(define-minor-mode my-override-mode
  "Overrides all major and minor mode keys" t)

(defvar my-override-map (make-sparse-keymap "my-override-map")
  "Override all major and minor mode keys")

(add-to-list 'emulation-mode-map-alists
             `((my-override-mode . ,my-override-map)))

(evil-define-key '(visual normal emacs) my-override-map (kbd "<left>")
  (lambda ()
    (interactive)
    (message "Use Vim keys: h for Left")))

(evil-define-key '(visual normal emacs) my-override-map (kbd "<right>")
  (lambda ()
    (interactive)
    (message "Use Vim keys: l for Right")))

(evil-define-key '(visual normal emacs) my-override-map (kbd "<up>")
  (lambda ()
    (interactive)
    (message "Use Vim keys: k for Up")))

(evil-define-key '(visual normal emacs) my-override-map (kbd "<down>")
  (lambda ()
    (interactive)
    (message "Use Vim keys: j for Down")))


(evil-make-intercept-map my-override-map '(normal visual emacs))



(keys/leader
 "e"   '(find-file :wk "Find File")
 "d"   '(dired :wk "Find folder")
 "w"   '(evil-window-map :wk "[w]indow"))

(keys/leader
 "b"   '(:wk "Buffers")
 "b b" '(consult-buffer :wk "Browse Buffers")
 "b d" '(kill-buffer :wk "Kill Buffer")
 "b i" '(ibuffer :wk "ibuffer")) 

(defun conf/init ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(keys/leader
 "f" '(:wk "Config Prefix")
 "f e" '(conf/init :wk "Open Init.el"))


(defun conf/open-org-directory ()
  (interactive)
  (dired org-directory))

(keys/leader
 "o" '(:wk "OrgMode Stuffs")
 "o A" '(org-agenda :wk "Org Agenda")
 "o o" '(conf/open-org-directory :wk "Org directory")
 "o f j" '(org-journal-find-file :wk "Find Journal File")
 "o j" '(org-journal-new-date-entry :wk "Org Journal Date Entry")
 "f O" '(( lambda () (interactive) (find-file (expand-file-name "~/Notes"))) :wk "Personal Notes")
 "f P" '(( lambda () (interactive) (find-file (expand-file-name "~/Org"))) :wk "Org Stuffs"))


(keys/leader
 :keymap 'org-journal-mode-map
 "o J" '(org-journal-previous-entry :wk "[O]rg [j]ournal previous entry"))

(keys/leader
 :keymaps 'org-mode-map
 "o" '(:wk "OrgMode Stuffs")
 "o e" '(org-edit-special :wk "Org src-block special edit")
 "o c" '(org-toggle-checkbox :wk "Toggle Checkbox")
 "o l" '(org-insert-link :wk "Insert Link"))

(keys/leader
 :keymap 'projectile-mode-map
 "p" '(:wk "Projects")
 "p a" '(projectile-add-known-project :wk "Add Project")
 "p p" '(projectile-switch-project :wk "Switch/open Project")
 "p R" '(projectile-remove-current-project-from-known-projects :wk "Remove Current Project")
 "p r" '(projectile-remove-known-project :wk "Remove Project")
 "p P"  '(projectile-command-map :wk "projectile Commands"))

(keys/leader
 "h" '(:wk "[H]elpful Commands")
 "h a" '(apropos-command :wk "All Description")
 "h f" '(describe-function :wk "[H]elpful describe [F]unction")
 "h k" '(describe-key :wk "[H]elpful Describe [k]ey")
 "h K" '(describe-keymap :wk "[H]elpful Describe [K]eymap")
 "h v" '(describe-variable :wk "[H]elpful Describe [v]ariable"))

(evil-define-key 'insert 'prog-mode-map (kbd "C-SPC") #'completion-at-point)
(evil-define-key 'insert 'corfu-map (kbd "<tab>") #'corfu-next)
(evil-define-key 'insert 'corfu-map (kbd "<backtab>") #'corfu-previous)

(keys/leader
 "c" '(:wk "Code")
 "c c" '(compile :wk "Compile")
 "c C" '(recompile :wk "Recompile")
 "c a" '(eglot-code-actions :wk "Code Actions")
 "c d" '(xref-find-definitions :wk "Find Definition")
 "c D" '(eglot-find-typeDefinition :wk "Find Type Definition")
 "c R" '(xref-find-references :wk "Find Implementations")
 "c r" '(eglot-rename :wk "Rename Symbol"))


(keys/leader
 "t" '(:wk "Toggleables")
 "t t" '(vterm-toggle-cd :wk "Toggle Terminal"))

