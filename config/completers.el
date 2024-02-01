(use-package vertico
  :custom
  (vertico-count 15)
  (vertico-resize t)
  (vertico-cycle t)
  :straight (vertico :files (:defaults "extensions/**")
		     :includes (vertico-indexed
				vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform))
  :config
  (vertico-mode)
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico-prescient
  :after vertico
  :straight t)

(use-package consult
  :straight t)

(use-package vertico-posframe
  :straight t
  :after (vertico vertico-prescient)
  :config (setq vertico-multiform-commands '((consult-line posframe
							   (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
							   (vertico-posframe-border-width . 10)
							   (vertico-posframe-fallback-mode . vertico-buffer-mode))
							   (t posframe)))
  (vertico-multiform-mode))

(use-package embark
  :straight t)

(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package marginalia
  :straight t
  :init (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful
  :straight t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-function)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable))
