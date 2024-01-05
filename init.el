;; -*- lexical-binding: t; -*-
(setq *config-dir* (expand-file-name "config/" user-emacs-directory))
(defun conf/load (file)
  (load (expand-file-name file *config-dir*)))

(conf/load "macros")
(conf/load "startup")
(conf/load "packages")
(conf/load "keymaps")
(conf/load "interface")
(conf/load "browsers")
(conf/load "folding")
(conf/load "file_browsers")
(conf/load "orgdoc")
(conf/load "programming")
(conf/load "prog_lang")
(conf/load "term")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" default))
 '(evil-undo-system 'undo-tree))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
