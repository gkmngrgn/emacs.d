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
      modus-themes-bold-constructs     t
      modus-themes-mixed-fonts         t
      modus-themes-subtle-line-numbers nil
      modus-themes-intense-mouseovers  nil
      modus-themes-deuteranopia        t
      modus-themes-tabs-accented       t
      modus-themes-variable-pitch-ui   nil
      modus-themes-inhibit-reload      t
      modus-themes-fringes             nil
      modus-themes-lang-checkers       '(straight-underline intense)
      modus-themes-mode-line           '(accented borderless)
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

(avy-setup-default)
(global-set-key (kbd "M-g g")   'avy-goto-char-2)
(global-set-key (kbd "M-g f")   'avy-goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)

;; mode-line
(straight-use-package 'smart-mode-line)

(setq sml/shorten-modes t)
(setq sml/name-width 20)

(sml/setup)

(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " Golden")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " tree-sitter")
(add-to-list 'sml/replacer-regexp-list '("^~/Workspace/" ":WS:") t)

;; code auto-complete
(straight-use-package 'company)
(straight-use-package 'company-jedi)
(straight-use-package 'company-lua)

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

(add-to-list 'company-backends #'(company-capf
                                  company-jedi
                                  company-lua))

;; flycheck
(straight-use-package 'flycheck)

(global-flycheck-mode)

;; search & find
(straight-use-package 'ctrlf)
(straight-use-package 'find-file-in-project)
(straight-use-package 'origami)
(straight-use-package 'rg)
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'which-key)

(which-key-mode)
(ctrlf-mode +1)
(global-origami-mode)
(rg-enable-default-bindings)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(setq ffip-use-rust-fd t)

(global-set-key (kbd "C-x C-d") 'ffip)
(global-set-key (kbd "C-c l f") 'origami-toggle-node)

;; lsp
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-origami)
(straight-use-package 'lsp-pyright)
(straight-use-package 'lsp-treemacs)
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

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c u")                 #'lsp-ui-imenu))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'jedi)
  (add-to-list 'lsp-disabled-clients 'rls)

  (add-to-list 'lsp-enabled-clients 'bash-ls)
  (add-to-list 'lsp-enabled-clients 'clangd)
  (add-to-list 'lsp-enabled-clients 'clojure-lsp)
  (add-to-list 'lsp-enabled-clients 'css-ls)
  (add-to-list 'lsp-enabled-clients 'dockerfile-ls)
  (add-to-list 'lsp-enabled-clients 'html-ls)
  (add-to-list 'lsp-enabled-clients 'json-ls)
  (add-to-list 'lsp-enabled-clients 'pyright)
  (add-to-list 'lsp-enabled-clients 'rust-analyzer)
  (add-to-list 'lsp-enabled-clients 'yamlls)

  (lsp-treemacs-sync-mode 1)

  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(add-hook 'lsp-mode-hook        #'lsp-enable-which-key-integration)
(add-hook 'c-mode-hook          #'lsp-deferred)
(add-hook 'c++-mode-hook        #'lsp-deferred)
(add-hook 'clojure-mode-hook    #'lsp-deferred)
(add-hook 'gdscript-mode-hook   #'lsp-deferred)
(add-hook 'go-mode-hook         #'lsp-deferred)
(add-hook 'javascript-mode-hook #'lsp-deferred)
(add-hook 'json-mode-hook       #'lsp-deferred)
(add-hook 'python-mode-hook     (lambda () (require 'lsp-pyright) (lsp)))
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

;; clojure
(straight-use-package 'cider)

;; python
(straight-use-package 'poetry)
(straight-use-package 'conda)

;; FILE MODES
(straight-use-package 'bazel)
(straight-use-package 'clojure-mode)
(straight-use-package 'cmake-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'gdscript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'json-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'web-mode)
(straight-use-package 'yaml-mode)

;; markdown
(setq markdown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

;; web
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset   2)
(setq web-mode-css-indent-offset    2)
(setq web-mode-script-padding       0)
(setq mmm-submode-decoration-level  0)
(setq js-indent-level               2)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
