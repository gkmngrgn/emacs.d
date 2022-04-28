;;; init.el --- GOEDEV personal emacs configuration file  -*- lexical-binding: t -*-

;; Copyright (c) 2010-2022 Gökmen Görgen
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


;; INIT
(if (display-graphic-p)
    (require 'init-gui)
  (require 'init-tui))


;; THEME
(straight-use-package 'modus-themes)

(setq modus-themes-italic-constructs   t
      modus-themes-bold-constructs     nil
      modus-themes-mixed-fonts         nil
      modus-themes-subtle-line-numbers nil
      modus-themes-intense-mouseovers  nil
      modus-themes-deuteranopia        t
      modus-themes-tabs-accented       t
      modus-themes-variable-pitch-ui   nil
      modus-themes-inhibit-reload      t
      modus-themes-fringes             nil
      modus-themes-lang-checkers       '(straight-underline intense)
      modus-themes-mode-line           '(accented borderless (padding . 4) (height . 0.9))
      modus-themes-markup              '(background italic)
      modus-themes-syntax              '(faint yellow-comments green-strings alt-syntax)
      modus-themes-hl-line             '(accented)
      modus-themes-paren-match         '(bold intense)
      modus-themes-links               '(neutral-underline background)
      modus-themes-box-buttons         '(variable-pitch flat faint 0.9)
      modus-themes-prompts             '(intense bold)
      modus-themes-completions         '((matches   . (extrabold))
                                         (selection . (semibold accented))
                                         (popup     . (accented intense)))
      modus-themes-mail-citations      nil
      modus-themes-region              '(bg-only no-extend)
      modus-themes-diffs               'desaturated
      modus-themes-org-blocks          'gray-background
      modus-themes-headings            '((1 . (overline background variable-pitch 1.3))
                                         (2 . (rainbow overline 1.1))
                                         (t . (semibold))))

(modus-themes-load-vivendi)

;; switch theme easily.
(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (modus-themes-toggle)
                  (if (display-graphic-p) (my-gui-change))))


;; NAVIGATION
(straight-use-package 'avy)
(straight-use-package 'ibuffer-vc)
(straight-use-package 'dirvish)

(avy-setup-default)
(global-set-key (kbd "M-g g")   'avy-goto-char-2)
(global-set-key (kbd "M-g f")   'avy-goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-d") 'dirvish)

(add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)

;; mode-line
(straight-use-package 'smart-mode-line)

(setq sml/shorten-modes t)
(setq sml/name-width 20)

(sml/setup)

(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " Golden")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " ivy")
(add-to-list 'sml/hidden-modes " tree-sitter")
(add-to-list 'sml/replacer-regexp-list '("^~/Workspace/" ":WS:") t)

;; code auto-complete
(straight-use-package 'company)
(straight-use-package 'company-tabnine)

(setq company-dabbrev-ignore-case       t)
(setq company-dabbrev-code-ignore-case  t)
(setq company-idle-delay                0.5)
(setq company-minimum-prefix-length     2)
(setq company-require-match             'never)
(setq company-show-numbers              t)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above   nil)
(setq company-tooltip-limit             10)

(global-company-mode)

(global-set-key (kbd "TAB") 'company-indent-or-complete-common)

(add-to-list 'company-backends #'(company-capf company-tabnine))

;; flycheck
(straight-use-package 'flycheck)

(global-flycheck-mode)

;; command completion
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'counsel-tramp)
(straight-use-package 'swiper)
(straight-use-package 'which-key)

(ivy-mode)
(which-key-mode)

(setq ivy-use-virtual-buffers      t)
(setq enable-recursive-minibuffers nil)
(setq search-default-mode          #'char-fold-to-regexp)

(global-set-key "\C-s"          'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>")    'ivy-resume)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key (kbd "C-x b")   'counsel-switch-buffer)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f")  'counsel-describe-function)
(global-set-key (kbd "<f1> v")  'counsel-describe-variable)
(global-set-key (kbd "<f1> o")  'counsel-describe-symbol)
(global-set-key (kbd "<f1> l")  'counsel-find-library)
(global-set-key (kbd "<f2> i")  'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u")  'counsel-unicode-char)
(global-set-key (kbd "C-c e")   'counsel-flycheck)
(global-set-key (kbd "C-c g")   'counsel-git)
(global-set-key (kbd "C-c j")   'counsel-git-grep)
(global-set-key (kbd "C-c k")   'counsel-rg)

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; lsp
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-dart)
(straight-use-package 'lsp-ivy)
(straight-use-package 'dap-mode)

(setq lsp-completion-provider :capf)
(setq lsp-enable-snippet nil)  ; company is better
(setq lsp-headerline-breadcrumb-icons-enable nil)
(setq lsp-idle-delay 0.500)
(setq lsp-keymap-prefix "C-c l")
(setq lsp-log-io nil)  ; if set to true can cause a performance hit
(setq lsp-modeline-code-actions-segments '(name))
(setq lsp-prefer-flymake nil)  ; flycheck is better
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-signature-doc-lines 10)
(setq lsp-signature-auto-activate nil)

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
  (add-to-list 'lsp-disabled-clients '(pyls rls)))

(add-hook 'lsp-mode-hook        #'lsp-enable-which-key-integration)
(add-hook 'c-mode-hook          #'lsp-deferred)
(add-hook 'c++-mode-hook        #'lsp-deferred)
(add-hook 'csharp-mode-hook     #'lsp-deferred)
(add-hook 'dart-mode-hook       #'lsp-deferred)
(add-hook 'gdscript-mode-hook   #'lsp-deferred)
(add-hook 'go-mode-hook         #'lsp-deferred)
(add-hook 'javascript-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook     #'lsp-deferred)
(add-hook 'rust-mode-hook       #'lsp-deferred)
(add-hook 'sh-mode-hook         #'lsp-deferred)
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
(straight-use-package 'unfill)
(straight-use-package 'hl-todo)

(with-eval-after-load (require 'smartparens-config))

(smartparens-global-mode t)
(show-smartparens-global-mode t)
(sp-local-pair 'web-mode "{" "}" :actions nil)

(global-tree-sitter-mode)

(add-hook 'prog-mode-hook            #'rainbow-delimiters-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(global-set-key (kbd "C-c w") 'er/expand-region)
(global-set-key (kbd "C-c q") 'unfill-paragraph)

(global-hl-todo-mode)

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

(golden-ratio-mode 1)

(setq golden-ratio-auto-scale t)

(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x O")     'prev-window)
(global-set-key (kbd "C-<left>")  'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<right>") 'tab-bar-switch-to-next-tab)

;; focusing
(straight-use-package 'focus)

(global-set-key (kbd "C-c f") 'focus-mode)

;; python
(straight-use-package 'poetry)
(straight-use-package 'conda)

;; FILE MODES
(straight-use-package 'bazel)
(straight-use-package 'cmake-mode)
(straight-use-package 'csharp-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'dart-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'gdscript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'json-mode)
(straight-use-package 'kotlin-mode)
(straight-use-package 'lua-mode)
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
