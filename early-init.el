;;; early-init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; UNICODE
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment   'utf-8)

;; INTERFACE
(global-auto-revert-mode)

(menu-bar-mode           0)
(electric-pair-mode      1)
(delete-selection-mode   1)
(temp-buffer-resize-mode t)
(mouse-wheel-mode        0)


;; ALIASES & VARIABLES
(defvar custom-file-path (expand-file-name "custom.el" user-emacs-directory))

;; CUSTOMISATIONS
(setq-default truncate-lines   t
              indent-tabs-mode nil)

(setq ring-bell-function              'ignore
      inhibit-splash-screen           t
      initial-scratch-message         nil
      select-enable-primary           t
      select-enable-clipboard         t
      max-mini-window-height          3
      warning-minimum-level           :error

      ;; MODE LINE
      line-number-mode                t
      column-number-mode              t

      ;; DISABLE TAB BAR
      tab-bar-close-button-show       nil
      tab-bar-mode                    nil
      tab-bar-show                    nil

      ;; SCROLLING
      scroll-margin                   0
      scroll-conservatively           100000
      scroll-preserve-screen-position 1
      auto-window-vscroll             nil

      ;; EDIT MODE
      require-final-newline           t

      ;; PERFORMANCE
      gc-cons-threshold               100000000
      read-process-output-max         (* 1024 1024)  ;; 1mb

      ;; BACKUP
      backup-directory-alist         `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory              "~/"
      create-lockfiles               nil
      custom-file                    custom-file-path

      ;; PACKAGE ARCHIVES
      package-archives               '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                                       ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                                       ("melpa"    . "https://melpa.org/packages/"))
      package-archive-priorities     '(("gnu-elpa" . 0)
                                       ("jcs-elpa" . 5)
                                       ("melpa"    . 10)))

;;; early-init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
