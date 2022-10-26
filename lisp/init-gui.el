;;; init-gui.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2022 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://git.gokmengorgen.net/goedev/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(defvar my-font      "IBM Plex Mono")
(defvar my-font-size 160)

(defun my-gui-change ()
  "Load my gui change."
  (set-frame-font my-font)
  (set-face-attribute 'default nil
                      :height my-font-size
                      :font my-font))

(setq initial-frame-alist    '((top    . 60) (left  . 15)
                               (height . 42) (width . 120))
      mouse-drag-copy-region nil
      select-enable-primary  nil)

(my-gui-change)
(fringe-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; add missing paths before starting the emacs.
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "LSP_USE_PLISTS")
        exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(provide 'init-gui)

;;; init-gui.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
