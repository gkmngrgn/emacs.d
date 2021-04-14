;;; early-init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2021 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; Package Manager Settings
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

;; Interface Settings
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)

(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Scroll Settings
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)
(setq auto-window-vscroll nil)

;; Performance
(setq gc-cons-threshold 600000000)
(setq read-process-output-max (* 4096 1024))  ; 4mb

;; Add lisp folder to the load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Backup
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory "~/")

(defvar custom-file-path "~/.emacs.d/custom.el")
(setq custom-file custom-file-path)
(when (file-exists-p custom-file-path)
  (load custom-file))

;;; early-init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
