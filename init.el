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

;; packages
(straight-use-package 'avy)
(straight-use-package 'bazel)
(straight-use-package 'cider)
(straight-use-package 'clojure-mode)
(straight-use-package 'cmake-mode)
(straight-use-package 'conda)
(straight-use-package 'company)
(straight-use-package 'company-jedi)
(straight-use-package 'company-lua)
(straight-use-package 'csv-mode)
(straight-use-package 'ctrlf)
(straight-use-package 'dap-mode)
(straight-use-package 'deadgrep)
(straight-use-package 'diff-hl)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'expand-region)
(straight-use-package 'find-file-in-project)
(straight-use-package 'flycheck)
(straight-use-package 'gdscript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'golden-ratio)
(straight-use-package 'hl-todo)
(straight-use-package 'ibuffer-vc)
(straight-use-package 'json-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-origami)
(straight-use-package 'lsp-pyright)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'lsp-ui)
(straight-use-package 'lua-mode)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'modus-themes)
(straight-use-package 'multiple-cursors)
(straight-use-package 'origami)
(straight-use-package 'password-store)
(straight-use-package 'poetry)
(straight-use-package 'puni)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'rust-mode)
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)
(straight-use-package 'smart-mode-line)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'unfill)
(straight-use-package 'web-mode)
(straight-use-package 'which-key)
(straight-use-package 'yaml-mode)

;; KEYMAPS
(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(defun toggle-theme ()
  "Toggle light/dark theme with a shortcut."
  (interactive)
  (modus-themes-toggle)
  (if (display-graphic-p)
      (my-gui-change)))

(global-set-key (kbd "C-c SPC") 'comment-line)                      ;; comment/uncomment line.
(global-set-key (kbd "TAB")     'company-indent-or-complete-common)
(global-set-key (kbd "<f5>")    'toggle-theme)
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
(global-set-key (kbd "C-c l f") 'origami-toggle-node)
(global-set-key (kbd "C-c w")   'er/expand-region)
(global-set-key (kbd "C-c q")   'unfill-paragraph)
(global-set-key (kbd "C-x O")   'prev-window)
(global-set-key (kbd "C-z")     'undo-only)

(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map (kbd "C-c u")                 #'lsp-ui-imenu))

;; HOOKS
(defun lsp-for-python ()
  "Activate pyright in Python files."
  (require 'lsp-pyright)
  (lsp))

(add-hook 'after-init-hook           'global-company-mode)
(add-hook 'before-save-hook          'delete-trailing-whitespace)
(add-hook 'c-mode-hook               'lsp-deferred)
(add-hook 'c++-mode-hook             'lsp-deferred)
(add-hook 'clojure-mode-hook         'lsp-deferred)
(add-hook 'gdscript-mode-hook        'lsp-deferred)
(add-hook 'go-mode-hook              'lsp-deferred)
(add-hook 'ibuffer-hook              'ibuffer-vc-set-filter-groups-by-vc-root)
(add-hook 'js-mode-hook              'lsp-deferred)
(add-hook 'json-mode-hook            'lsp-deferred)
(add-hook 'lsp-mode-hook             'lsp-enable-which-key-integration)
(add-hook 'magit-pre-refresh-hook    'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook   'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook            'rainbow-delimiters-mode)
(add-hook 'python-mode-hook          'lsp-for-python)
(add-hook 'rust-mode-hook            'lsp-deferred)
(add-hook 'sh-mode-hook              'lsp-deferred)
(add-hook 'term-mode-hook            'puni-disable-puni-mode)
(add-hook 'text-mode-hook            'visual-line-mode)
(add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)
(add-hook 'yaml-mode-hook            'lsp-deferred)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook     'lsp-origami-try-enable))

;; INIT GUI OR TUI
(if (display-graphic-p)
    (require 'init-gui)
  (require 'init-tui))

;; THEME
(setq modus-themes-italic-constructs   nil
      modus-themes-bold-constructs     t
      modus-themes-mixed-fonts         t
      modus-themes-subtle-line-numbers nil
      modus-themes-intense-mouseovers  nil
      modus-themes-deuteranopia        nil
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

;; MODE-LINE
(setq sml/shorten-modes t)
(setq sml/name-width 20)

(sml/setup)

(add-to-list 'sml/hidden-modes         " ElDoc")
(add-to-list 'sml/hidden-modes         " Golden")
(add-to-list 'sml/hidden-modes         " company")
(add-to-list 'sml/hidden-modes         " tree-sitter")

(add-to-list 'sml/replacer-regexp-list '("^~/Workspace/" ":WS:" ) t)
(add-to-list 'sml/replacer-regexp-list '("^:WS:mimi/"    ":MM:" ) t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:ORG/"    ":ORG:") t)

;; AUTO-COMPLETE
(setq company-dabbrev-ignore-case       t)
(setq company-dabbrev-code-ignore-case  t)
(setq company-idle-delay                0.5)
(setq company-minimum-prefix-length     2)
(setq company-require-match             'never)
(setq company-show-numbers              t)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above   nil)
(setq company-tooltip-limit             10)

(define-abbrev-table 'global-abbrev-table
  '(
    ("afaik" "as far as I know")
    ("asap"  "as soon as possible")
    ("btw"   "by the way")))

(setq company-backends '((company-capf          ;; sort backends by priority.
                          company-keywords
                          company-files
                          company-elisp
                          company-jedi
                          company-lua
                          company-abbrev
                          company-dabbrev
                          company-dabbrev-code)))

;; EDITOR EXTENSIONS
(setq ffip-use-rust-fd t)
(setq golden-ratio-auto-scale t)

(avy-setup-default)
(which-key-mode)
(golden-ratio-mode 1)
(ctrlf-mode +1)
(electric-pair-mode 1)

(menu-bar-mode 0)

(delete-selection-mode 1)
(temp-buffer-resize-mode t)

(global-auto-revert-mode)
(global-flycheck-mode)
(global-hl-line-mode)
(global-hl-todo-mode)
(global-origami-mode)
(global-tree-sitter-mode)
(puni-global-mode)

(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

(conda-env-autoactivate-mode t)

;; LSP
(setq lsp-completion-provider :capf)
(setq lsp-enable-snippet nil)                     ; company is better
(setq lsp-headerline-breadcrumb-icons-enable nil)
(setq lsp-idle-delay 0.500)
(setq lsp-keymap-prefix "C-c l")
(setq lsp-log-io nil)                             ; if set to true can cause a performance hit
(setq lsp-modeline-code-actions-segments '(name))
(setq lsp-prefer-flymake nil)                     ; flycheck is better
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-signature-doc-lines 10)
(setq lsp-signature-auto-activate nil)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'jedi)
  (add-to-list 'lsp-disabled-clients 'jsts-ls)
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
  (add-to-list 'lsp-enabled-clients 'ts-ls)
  (add-to-list 'lsp-enabled-clients 'yamlls)

  (lsp-treemacs-sync-mode 1))

;; GIT
(global-diff-hl-mode)
(diff-hl-margin-mode)

;; ORG-MODE
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))
(setq org-log-done t)
(setq org-agenda-files '("~/Documents/ORG/home.org"
                         "~/Documents/ORG/open.org"
                         "~/Documents/ORG/mimi.org"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; MARKDOWN
(setq markdown-command "multimarkdown")

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

;; WEB
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
