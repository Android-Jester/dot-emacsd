(setq gc-cons-threshold (* 1024 1024 500)
	  ;;vc-follow-symlinks nil)
	  package-enable-at-startup nil)
;; (setq read-process-output-max (* (* 1024 2) 1024))

;; (setq gc-cons-threshold most-positive-fixnum
;;       file-name-handler-alist nil
;;       site-run-file nil)

;; (setq gc-cons-threshold (* 2 1000 1000))

;; (defvar perm/gc-cons-threshold 100000000)

;; (add-hook 'emacs-startup-hook
;;           (lambda () (setq gc-cons-threshold perm/gc-cons-threshold
;;                            gc-cons-percentage 0.1
;;                            file-name-handler-alist file-name-handler-alist-original)))

;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (setq gc-cons-threshold (* perm/gc-cons-threshold 2))))

;; (add-hook 'minibuffer-exit-hook (lambda ()
;;                                   (garbage-collect)
;;                                   (setq gc-cons-threshold perm/gc-cons-threshold)))

;; (add-hook 'emacs-startup-hook (lambda ()
;;                                 (message "Emacs loaded in %s with %d garbage collections."
;;                                          (format "%.2f " (float-time (time-subtract after-init-time before-init-time))) gcs-done)))
