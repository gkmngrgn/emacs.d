;;; goedev.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2023 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(require 'package)

(setq is-package-initialized nil)

(defun goedev/switch-to-previous-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(defun goedev/switch-to-default-buffer-group ()
  "Switch to default buffer group."
  (ibuffer-switch-to-saved-filter-groups "default"))

(defun goedev/install-packages (packages)
  "Initialize package manager and install packages."
  (unless is-package-initialized
    (progn
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      (package-initialize)
      (message "goedev => package initialized.")
      (setq is-package-initialized t)))

  (when packages
    (let ((package (car packages)))
      (unless (package-installed-p package)
        (package-install package)))
    (goedev/install-packages (cdr packages))))

(defun goedev/configure-gui ()
  "Configure gui when you need to run Emacs with GUI."

  (defvar my-font "IBM Plex Mono")
  (defvar my-font-size 130)

  (set-frame-font my-font)

  (set-face-attribute 'default nil
                      :height my-font-size
                      :font my-font)
  (set-face-attribute 'fixed-pitch nil
                      :height my-font-size
                      :font my-font)

  (set-face-attribute 'mode-line nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil)

  (fringe-mode 0)
  (scroll-bar-mode 0)
  (tool-bar-mode 0))

(provide 'goedev)

;;; goedev.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
