;;; early-init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; UNICODE
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)

;; ALIASES
(defalias 'yes-or-no-p 'y-or-n-p)

;; INTERFACE
(global-auto-revert-mode)

(menu-bar-mode 0)
(electric-pair-mode 1)
(delete-selection-mode 1)
(temp-buffer-resize-mode t)
(xterm-mouse-mode 1)

(setq-default truncate-lines t
              indent-tabs-mode nil
              tab-width 2)

(setq ring-bell-function 'ignore
      inhibit-splash-screen t
      initial-scratch-message nil
      select-enable-primary t
      select-enable-clipboard t
      max-mini-window-height 3
      warning-minimum-level :error
      ;; mode line
      line-number-mode t
      column-number-mode t
      ;; disable tab bar
      tab-bar-close-button-show nil
      tab-bar-mode nil
      tab-bar-show nil)

;; SCROLLING
(scroll-bar-mode 0)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil)

;; EDIT MODE
(set-default 'abbrev-mode t)
(setq-default fill-column 88)
(setq require-final-newline t)

;; PERFORMANCE
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))  ;; 1mb

;; BACKUP
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory "~/"
      create-lockfiles nil)

(defvar custom-file-path (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file custom-file-path)

;;; early-init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
