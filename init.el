;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2022 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://git.goe.dev/goedev/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; PACKAGE MANAGER
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; PACKAGES
(package-install 'avy)
(package-install 'company)
(package-install 'company-prescient)
(package-install 'ctrlf)
(package-install 'deadgrep)
(package-install 'diff-hl)
(package-install 'dirvish)
(package-install 'eglot)
(package-install 'expand-region)
(package-install 'find-file-in-project)
(package-install 'magit)
(package-install 'minions)
(package-install 'modus-themes)
(package-install 'multiple-cursors)
(package-install 'poetry)
(package-install 'puni)
(package-install 'rainbow-delimiters)
(package-install 'selectrum)
(package-install 'selectrum-prescient)
(package-install 'tree-sitter)
(package-install 'tree-sitter-langs)
(package-install 'unfill)

;; FILE MODES
(package-install 'cmake-mode)
(package-install 'csv-mode)
(package-install 'dockerfile-mode)
(package-install 'gdscript-mode)
(package-install 'go-mode)
(package-install 'json-mode)
(package-install 'lua-mode)
(package-install 'markdown-mode)
(package-install 'rust-mode)
(package-install 'typescript-mode)
(package-install 'web-mode)
(package-install 'yaml-mode)

;; THEME
(if (display-graphic-p)
    ;; GUI SETTINGS
    (progn
      (set-frame-font "IBM Plex Mono")
      (set-face-attribute 'default nil
                          :height  160
                          :font    "IBM Plex Mono")
      (fringe-mode     0)
      (scroll-bar-mode 0)
      (tool-bar-mode   0))

  ;; TUI SETTINGS
  (progn
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)))

(modus-themes-load-themes)
(modus-themes-load-vivendi)

;; KEYMAPS
(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-c SPC") 'comment-line)                      ;; comment/uncomment line.
(global-set-key (kbd "TAB")     'company-indent-or-complete-common)
(global-set-key (kbd "<f5>")    'modus-themes-toggle)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)                     ;; line number replacement.
(global-set-key (kbd "M-g g")   'avy-goto-char-2)
(global-set-key (kbd "C-c s")   'deadgrep)
(global-set-key (kbd "C-x C-b") 'ibuffer)                           ;; default buffer replacement.
(global-set-key (kbd "C-x C-d") 'ffip)
(global-set-key (kbd "C-c m")   'mc/edit-lines)
(global-set-key (kbd "C-c j")   'mc/mark-next-like-this)
(global-set-key (kbd "C-c k")   'mc/mark-previous-like-this)
(global-set-key (kbd "C-c n")   'mc/mark-all-like-this)
(global-set-key (kbd "C-c w")   'er/expand-region)
(global-set-key (kbd "C-c q")   'unfill-paragraph)
(global-set-key (kbd "C-x O")   'prev-window)
(global-set-key (kbd "C-z")     'undo-only)
(global-set-key (kbd "C-c l l") 'eglot)
(global-set-key (kbd "C-c l r") 'eglot-rename)
(global-set-key (kbd "C-c l f") 'eglot-format)
(global-set-key (kbd "C-c l n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c l p") 'flymake-goto-prev-error)

;; HOOKS
(add-hook   'after-init-hook           'global-company-mode)
(add-hook   'before-save-hook          'delete-trailing-whitespace)
(add-hook   'magit-pre-refresh-hook    'diff-hl-magit-pre-refresh)
(add-hook   'magit-post-refresh-hook   'diff-hl-magit-post-refresh)
(add-hook   'prog-mode-hook            'rainbow-delimiters-mode)
(add-hook   'term-mode-hook            'puni-disable-puni-mode)
(add-hook   'text-mode-hook            'visual-line-mode)
(add-hook   'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
(add-hook   'ibuffer-mode-hook         (lambda ()
                                         (ibuffer-switch-to-saved-filter-groups "default")))

(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook           'magit-after-save-refresh-status t))

;; FILE MODES
(add-to-list 'auto-mode-alist '("\\.html?\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; EDITOR EXTENSIONS
(minions-mode)
(avy-setup-default)
(ctrlf-mode +1)
(dirvish-override-dired-mode)
(global-tree-sitter-mode)
(puni-global-mode)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(company-prescient-mode +1)
(prescient-persist-mode +1)

(global-diff-hl-mode)
(diff-hl-margin-mode)

;; TRAMP SETTINGS
(require 'tramp)

(add-to-list 'tramp-remote-path "/opt/homebrew/bin/")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
