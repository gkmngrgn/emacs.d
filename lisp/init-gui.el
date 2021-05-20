;;; init-gui.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2021 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(defun hdpi? ()
  "Check if you're using alienware or thinkpad."
  (and (= 3840 (display-pixel-width))
       (= 2160 (display-pixel-height))))

(defun my-gui-change ()
  "Load my gui change."
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
                      :box nil))

(defvar my-font "Iosevka Term")

(if (hdpi?)
    ;; HiDPI settings
    (defvar my-font-size 130)

  ;; Default DPI settings
  (defvar my-font-size 120))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(my-gui-change)
(fringe-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

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
