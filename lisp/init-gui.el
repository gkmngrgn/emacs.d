;;; init-gui.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2021 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(defvar font-default "Iosevka Term")

(defun hdpi? ()
  "Check if you're using alienware or thinkpad."
  (and (= 3840 (display-pixel-width))
       (= 2160 (display-pixel-height))))

(fringe-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(set-frame-font font-default)

(if (hdpi?)
    ;; HiDPI settings
    (set-face-attribute 'default nil :height 120)

  ;; Default DPI settings
  (set-face-attribute 'default nil :height 110))

;; add missing paths before starting the emacs.
(when (memq window-system '(mac ns x))
  (straight-use-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(provide 'init-gui)

;;; init-gui.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
