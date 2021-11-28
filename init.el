;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2021 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/emacs.d/

;;; Commentary:

;; Take a look at README.md file for more information.

;;; Code:

(menu-bar-mode 0)
(global-hl-line-mode)
(global-auto-revert-mode)
(delete-selection-mode 1)
(temp-buffer-resize-mode t)

;; UTF-8
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment   'utf-8)

;; KEYMAPS
(global-set-key (kbd "C-c SPC") 'comment-line)

(defalias 'yes-or-no-p 'y-or-n-p)

;; HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook   'visual-line-mode)

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

;; theme
(straight-use-package 'modus-themes)
(setq modus-themes-slanted-constructs t)
(setq modus-themes-bold-constructs nil)

(modus-themes-load-vivendi)

(if (display-graphic-p)
    (require 'init-gui))

;; make gui look like in the terminal.
(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (modus-themes-toggle)
                  (if (display-graphic-p) (my-gui-change))))

(straight-use-package 'all-the-icons)

;; navigation
(straight-use-package 'avy)

(avy-setup-default)
(global-set-key (kbd "M-g g") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)

;; mode-line
(straight-use-package 'smart-mode-line)

(setq sml/theme 'respectful)
(setq sml/shorten-modes t)
(setq sml/name-width 20)
(setq sml/mode-width 'right)

(sml/setup)

(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " ivy")
(add-to-list 'sml/hidden-modes " tree-sitter")
(add-to-list 'sml/replacer-regexp-list '("^~/Workspace/" ":WS:") t)

;; code auto-complete
(straight-use-package 'company)

(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-code-ignore-case t)
(setq company-idle-delay 0.5)
(setq company-minimum-prefix-length 2)
(setq company-require-match 'never)
(setq company-show-numbers nil)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above nil)
(setq company-tooltip-limit 10)

(global-company-mode)

(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

(push 'company-capf company-backends)

;; flycheck
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-rust)

(global-flycheck-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; command completion
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(straight-use-package 'which-key)

(ivy-mode)
(which-key-mode)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers nil)
(setq search-default-mode #'char-fold-to-regexp)

(global-set-key (kbd "C-s")     'swiper)
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
(global-set-key (kbd "C-x C-d") 'counsel-git)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key (kbd "C-c e")   'counsel-flycheck)
(global-set-key (kbd "C-c j")   'counsel-git-grep)
(global-set-key (kbd "C-c k")   'counsel-rg)

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; lsp
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-dart)
(straight-use-package 'lsp-ivy)

(setq lsp-completion-provider :capf)
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-prefer-flymake nil)  ; flycheck is better
(setq lsp-modeline-code-actions-segments '(name))
(setq lsp-keymap-prefix "C-c l")
(setq lsp-idle-delay 0.500)
(setq lsp-enable-snippet nil)  ; company is better
(setq lsp-log-io nil)  ; if set to true can cause a performance hit
(setq lsp-signature-doc-lines 10)
(setq lsp-signature-auto-activate nil)
(setq lsp-ui-doc-enable nil)

(setq lsp-pylsp-plugins-autopep8-enabled nil)
(setq lsp-pylsp-plugins-flake8-enabled nil)
(setq lsp-pylsp-plugins-mccabe-enabled nil)
(setq lsp-pylsp-plugins-pycodestyle-enabled nil)
(setq lsp-pylsp-plugins-pylint-enabled nil)
(setq lsp-pylsp-plugins-rope-completion-enabled nil)
(setq lsp-pylsp-plugins-yapf-enabled nil)

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c u")                 #'lsp-ui-imenu))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients 'pyls))

(add-hook 'lsp-mode-hook        #'lsp-enable-which-key-integration)
(add-hook 'c-mode-hook          #'lsp-deferred)
(add-hook 'csharp-mode-hook     #'lsp-deferred)
(add-hook 'dart-mode-hook       #'lsp-deferred)
(add-hook 'gdscript-mode-hook   #'lsp-deferred)
(add-hook 'go-mode-hook         #'lsp-deferred)
(add-hook 'javascript-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook     #'lsp-deferred)
(add-hook 'rust-mode-hook       #'lsp-deferred)
(add-hook 'yaml-mode-hook       #'lsp-deferred)

;; git
(straight-use-package 'diff-hl)
(straight-use-package 'magit)

(global-diff-hl-mode)
(diff-hl-margin-mode)

(add-hook 'magit-pre-refresh-hook  'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; org-mode customizations
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))
(setq org-log-done t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; editing
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'expand-region)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'smartparens)
(straight-use-package 'undo-fu)
(straight-use-package 'unfill)
(straight-use-package 'origami)

(with-eval-after-load (require 'smartparens-config))

(smartparens-global-mode t)
(show-smartparens-global-mode t)
(sp-local-pair 'web-mode "{" "}" :actions nil)

(global-tree-sitter-mode)
(global-origami-mode)

(add-hook 'prog-mode-hook            #'rainbow-delimiters-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-_"))
(global-set-key   (kbd "C-z")         'undo-fu-only-undo)
(global-set-key   (kbd "C-M-z")       'undo-fu-only-redo)

(global-set-key   (kbd "C-M-o")       'origami-toggle-node)
(global-set-key   (kbd "C-M-q")       'unfill-paragraph)
(global-set-key   (kbd "C-M-w")       'er/expand-region)

;; web
(straight-use-package 'web-mode)
(straight-use-package 'tide)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset   2)
(setq web-mode-css-indent-offset    2)
(setq web-mode-script-padding       0)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'"  . web-mode))

(defun setup-tide-mode ()
  "My custom tide setup."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'before-save-hook     'tide-format-before-save)
(add-hook 'typescript-mode-hook 'setup-tide-mode)

;; window management
(straight-use-package 'golden-ratio)

(setq tab-bar-close-button-show nil)
(setq tab-bar-mode t)
(setq tab-bar-show t)

(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-c f")     'golden-ratio)
(global-set-key (kbd "C-x O")     'prev-window)
(global-set-key (kbd "C-<left>")  'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<right>") 'tab-bar-switch-to-next-tab)

;; python
(straight-use-package 'poetry)
(straight-use-package 'pyenv-mode)

(pyenv-mode)

;; FILE MODES
(straight-use-package 'bazel)
(straight-use-package 'csharp-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'dart-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'gdscript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'json-mode)
(straight-use-package 'kotlin-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'powershell)
(straight-use-package 'rust-mode)
(straight-use-package 'typescript-mode)
(straight-use-package 'yaml-mode)

(setq markdown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

(setq mmm-submode-decoration-level 0)

(setq js-indent-level 2)
(setq typescript-indent-level 2)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
