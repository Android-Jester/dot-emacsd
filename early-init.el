(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      gc-cons-threshold (* 50 1024 1024)
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      user-full-name "Duah Kwadwo Adjei"
      user-mail-address "duah14@outlook.com"
      user-login-name "AndroidJester"
      auth-sources '("~/.authinfo.gpg")
      cache-dir (concat (getenv "HOME") "/.cache/emacs")
      config-dir (expand-file-name "config/" user-emacs-directory)
      warning-minimum-level :error
      frame-title-format '("Jester's Emacs")
      ring-bell-function 'ignore
      frame-resize-pixelwise t
      default-directory "~/Programming"
      inhibit-startup-message t
      inhibit-splash-screen t
      create-lockfiles nil
      make-backup-files nil
      native-comp-always-compile t)


