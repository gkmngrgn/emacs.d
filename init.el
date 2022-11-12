;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2022 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://git.gokmengorgen.net/goedev/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

;; GLOBAL SETTINGS

;; unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment   'utf-8)

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; PACKAGE MANAGER
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; PACKAGES
(straight-use-package 'avy)
(straight-use-package 'company)
(straight-use-package 'company-prescient)
(straight-use-package 'ctrlf)
(straight-use-package 'deadgrep)
(straight-use-package 'diff-hl)
(straight-use-package 'dirvish)
(straight-use-package 'eglot)
(straight-use-package 'expand-region)
(straight-use-package 'find-file-in-project)
(straight-use-package 'golden-ratio)
(straight-use-package 'magit)
(straight-use-package 'modus-themes)
(straight-use-package 'multiple-cursors)
(straight-use-package 'poetry)
(straight-use-package 'puni)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'unfill)

;; FILE MODES
(straight-use-package 'cmake-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'gdscript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'json-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'typescript-mode)
(straight-use-package 'web-mode)
(straight-use-package 'yaml-mode)

;; ENVIRONMENT VARIABLES
(setq exec-path-from-shell-variables           '("GOPATH" "LANG")
      exec-path-from-shell-check-startup-files nil)

(when (daemonp)
  (exec-path-from-shell-initialize))

;; THEME
(setq modus-themes-mode-line '(borderless))

(if (display-graphic-p)
    ;; GUI SETTINGS
    (progn
      (defvar my-font      "IBM Plex Mono")
      (defvar my-font-size 160)

      (setq initial-frame-alist    '((top    . 60) (left  . 15)
                                     (height . 42) (width . 120))
            mouse-drag-copy-region nil
            select-enable-primary  nil)

      (set-frame-font my-font)
      (set-face-attribute 'default nil
                          :height  my-font-size
                          :font    my-font)
      (fringe-mode     0)
      (scroll-bar-mode 0)
      (tool-bar-mode   0))

  ;; TUI SETTINGS
  (progn
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)))

(modus-themes-load-vivendi)

;; KEYMAPS
(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-c SPC") 'comment-line)                      ;; comment/uncomment line.
(global-set-key (kbd "TAB")     'company-indent-or-complete-common)
(global-set-key (kbd "<f5>")    'modus-themes-toggle)
(global-set-key (kbd "M-g f")   'avy-goto-line)                     ;; line number replacement.
(global-set-key (kbd "M-g g")   'avy-goto-char-2)
(global-set-key (kbd "C-c s")   'deadgrep)
(global-set-key (kbd "C-x C-b") 'ibuffer)                           ;; default buffer replacement.
(global-set-key (kbd "C-x C-d") 'ffip)
(global-set-key (kbd "C-c m")   'mc/edit-lines)
(global-set-key (kbd "C-c j")   'mc/mark-next-like-this)
(global-set-key (kbd "C-c k")   'mc/mark-previous-like-this)
(global-set-key (kbd "C-c n")   'mc/mark-all-like-this)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c w")   'er/expand-region)
(global-set-key (kbd "C-c q")   'unfill-paragraph)
(global-set-key (kbd "C-x O")   'prev-window)
(global-set-key (kbd "C-z")     'undo-only)

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

(when (executable-find "clangd")
  (add-hook 'c-mode-hook               'eglot-ensure)
  (add-hook 'c++-mode-hook             'eglot-ensure))

(when (executable-find "gopls")
  (add-hook 'go-mode-hook              'eglot-ensure))

(when (executable-find "lua-language-server")
  (add-hook 'lua-mode-hook             'eglot-ensure))

(when (executable-find "pyright")
  (add-hook 'python-mode-hook          'eglot-ensure))

(when (executable-find "rust-analyzer")
  (add-hook 'rust-mode-hook            'eglot-ensure))

(when (executable-find "typescript-language-server")
  (add-hook 'js-mode-hook              'eglot-ensure)
  (add-hook 'json-mode-hook            'eglot-ensure)
  (add-hook 'typescript-mode-hook      'eglot-ensure))

(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook           'magit-after-save-refresh-status t))

;; FILE MODES
(add-to-list 'auto-mode-alist '("\\.html?\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

;; AUTO-COMPLETE
(define-abbrev-table 'global-abbrev-table
  '(
    ("afaik" "as far as I know")
    ("asap"  "as soon as possible")
    ("btw"   "by the way")))

;; EDITOR EXTENSIONS
(setq ffip-use-rust-fd                t
      golden-ratio-auto-scale         t
      golden-ratio-max-width          100
      golden-ratio-adjust-factor      .8
      golden-ratio-wide-adjust-factor .8)

(avy-setup-default)
(golden-ratio-mode 1)
(ctrlf-mode +1)
(dirvish-override-dired-mode)
(global-tree-sitter-mode)
(puni-global-mode)
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(company-prescient-mode +1)
(prescient-persist-mode +1)

;; BUFFERS
(setq ibuffer-saved-filter-groups (quote (("default"
                                           ("VCS" (or (mode . magit-mode)
                                                      (mode . magit-status-mode)
                                                      (mode . magit-log-mode)
                                                      (mode . magit-process-mode)
                                                      (mode . magit-revision-mode)
                                                      (mode . magit-diff-mode)))
                                           ("Files"  (filename . ".*\.*$"))))))

;; GIT
(global-diff-hl-mode)
(diff-hl-margin-mode)

;; ORG-MODE
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE"))
      org-log-done      t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; MARKDOWN
(setq markdown-command "multimarkdown")

;; WEB
(setq web-mode-markup-indent-offset 2
      web-mode-code-indent-offset   2
      web-mode-css-indent-offset    2
      web-mode-script-padding       0
      mmm-submode-decoration-level  0
      js-indent-level               2)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
