(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

(defun rc/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (rc/require theme-package)
    (load-theme theme t)))

(rc/require 'dash)
(require 'dash)

(rc/require 'dash-functional)
(require 'dash-functional)


(defun rc/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'rc/colorize-compilation-buffer)

(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'rc/duplicate-line)


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

(provide 'rc) 
