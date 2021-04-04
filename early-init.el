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

(setq require-final-newline t)
(setq line-number-mode t)
(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Scroll Settings
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Performance
(setq gc-cons-threshold 600000000)
(setq read-process-output-max (* 4096 1024))  ; 4mb

;; Backup
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory "~/")

;;; early-init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
