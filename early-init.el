;;; early-init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2022 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://git.gokmengorgen.net/goedev/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; PACKAGE MANAGER
(setq comp-deferred-compilation-deny-list ()
      package-enable-at-startup           nil
      straight-use-package-by-default     t
      use-package-always-ensure           t)

;; INTERFACE
(setq-default truncate-lines   t
              indent-tabs-mode nil
              tab-width        2)

(setq ring-bell-function        'ignore
      inhibit-splash-screen     t
      initial-scratch-message   nil
      select-enable-primary     t
      select-enable-clipboard   t
      ;; mode line
      line-number-mode          t
      column-number-mode        t
      ;; disable tab bar
      tab-bar-close-button-show nil
      tab-bar-mode              nil
      tab-bar-show              nil)

;; SCROLLING
(setq scroll-margin                   0
      scroll-conservatively           100000
      scroll-preserve-screen-position 1
      auto-window-vscroll             nil)

;; EDIT MODE
(set-default 'abbrev-mode t)
(setq-default fill-column 88)
(setq require-final-newline t)

;; PERFORMANCE
(setq gc-cons-threshold       100000000
      read-process-output-max (* 1024 1024))  ;; 1mb

;; MODULES
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; BACKUP
(defvar custom-file-path "~/.emacs.d/custom.el")
(setq backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory              "~/"
      create-lockfiles               nil
      custom-file                    custom-file-path)

(when (file-exists-p custom-file-path)
  (load custom-file))

;;; early-init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
