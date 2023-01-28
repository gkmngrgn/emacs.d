;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2023 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(require 'goedev)

;; DEPENDENCIES
(goedev/install-packages '(avy
                           company
                           company-prescient
                           ctrlf
                           deadgrep
                           diff-hl
                           dirvish
                           eglot
                           expand-region
                           find-file-in-project
                           magit
                           minions
                           modus-themes
                           multiple-cursors
                           poetry
                           puni
                           rainbow-delimiters
                           selectrum
                           selectrum-prescient
                           tree-sitter
                           tree-sitter-langs
                           unfill))

;; FILE MODES
(goedev/install-packages '(cmake-mode
                           csv-mode
                           dockerfile-mode
                           gdscript-mode
                           go-mode
                           json-mode
                           lua-mode
                           markdown-mode
                           rust-mode
                           typescript-mode
                           web-mode
                           yaml-mode))

;; KEYMAPS
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-c SPC") 'comment-line)                      ;; comment/uncomment line.
(global-set-key (kbd "TAB")     'company-indent-or-complete-common)
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
(global-set-key (kbd "C-x O")   'goedev/switch-to-previous-window)
(global-set-key (kbd "C-z")     'undo-only)
(global-set-key (kbd "C-c l l") 'eglot)
(global-set-key (kbd "C-c l r") 'eglot-rename)
(global-set-key (kbd "C-c l f") 'eglot-format)
(global-set-key (kbd "C-c l n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c l p") 'flymake-goto-prev-error)

;; HOOKS
(add-hook 'after-init-hook           'global-company-mode)
(add-hook 'before-save-hook          'delete-trailing-whitespace)
(add-hook 'magit-pre-refresh-hook    'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook   'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook            'rainbow-delimiters-mode)
(add-hook 'term-mode-hook            'puni-disable-puni-mode)
(add-hook 'text-mode-hook            'visual-line-mode)
(add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
(add-hook 'ibuffer-mode-hook         'goedev/switch-to-default-buffer-group)

(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook         'magit-after-save-refresh-status t))

;; FILE MODES
(add-to-list 'auto-mode-alist '("\\.html?\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; EDITOR EXTENSIONS
(load-theme 'modus-vivendi :no-confirm)

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

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
