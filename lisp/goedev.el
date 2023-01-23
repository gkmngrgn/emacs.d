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

(provide 'goedev)

;;; goedev.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End: