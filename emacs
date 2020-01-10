;; -*- mode: Lisp;-*-

;; Setup packages
(require 'package)

;; Extra package repositories
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Initialize the package system
(package-initialize)

;; The use-package package needs to be installed explicitly
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Load ido
(use-package ido
  :ensure t
  :config
  (ido-mode t)
  :init
  (add-hook
   'ido-setup-hook
   (lambda()
     (define-key ido-completion-map (kbd "<tab>") 'ido-next-match))))

;; Load magit
(use-package magit
  :ensure t
  :bind
  ([f11] . magit)
  ("C-c b" . magit-blame)
  :config
  (setq smerge-command-prefix "\C-c"))

;; Load and use flycheck globally
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  :bind
  ("C-c n" . flycheck-next-error)
  ("C-c p" . flycheck-previous-error))

(use-package spaceline
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :config (spaceline-all-the-icons-theme))

;; Load custom theme
(use-package alect-themes
  :ensure t
  :config
  (load-theme 'alect-black-alt t))

;; Use webmode for editting webcode
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  :custom
  ;; Always use django engine for editing HTML files
  (web-mode-engines-alist '(("django" . "\\.html\\'")))
  ;; Saner indentation
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  ;; Highlight matching elements
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

;; Helm
(use-package helm
  :ensure t)

;; Ag
(use-package ag
  :ensure t)

;; Dockerfile mode for editing Dockerfile
(use-package dockerfile-mode
  :ensure t)

;; Elpy for python editing
(use-package elpy
  :ensure t
  :config
  (elpy-enable))

;; Mypy for Python type hinting check
(use-package flycheck-mypy
  :ensure t)

;; Bitbake mode
(use-package bitbake
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.bb?\\'" . bitbake-mode)))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  :bind
  ("C-c f" . projectile-ag))

;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode))

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (use-package lsp-ui
    :ensure t)
  (use-package company-lsp
    :ensure t)
  (use-package helm-lsp
    :ensure t)
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection
                                       "/usr/bin/clangd")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'clangd-remote)))
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
  :commands lsp)

;; Modern C++ font highlight
(use-package modern-cpp-font-lock
  :ensure t
  :hook
  (c++-mode . modern-c++-font-lock-mode))

;; Helm-projectile
(use-package helm-projectile
  :ensure t
  :bind
  ("C-c g" . helm-projectile))

;; YAML mode
(use-package yaml-mode
  :ensure t)

;; CMake mode
(use-package cmake-mode
  :ensure t
  :config
  (setq cmake-tab-width 4))

;; Markdown mode
(use-package markdown-mode
  :ensure t)

;; JSON mode
(use-package json-mode
  :ensure t)

;; Groovy mode
(use-package groovy-mode
  :ensure t)

;; Ini mode for configuration files
(use-package ini-mode
  :ensure t)

;; Isortify for python imports
(use-package isortify
  :ensure t)

;; Get rid of annoying GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Get rid of annoying splash screen
(setq inhibit-splash-screen t)

;; Get rid of annoying bell
(setq ring-bell-function 'ignore)

;; Use spaces not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ensure UTF-8
(set-language-environment "UTF-8")

;; Set the default font
(set-frame-font "Hack-10" nil t)

;; Custom global key bindings
(global-set-key [f8] 'goto-line)
(global-set-key [f9] 'projectile-compile-project)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Switch between header/implementation file in C and C++
(add-hook
 'c-mode-common-hook
 (lambda()
   (local-set-key (kbd "C-c h") 'ff-find-other-file)))

;; Always scroll compilation buffer
(setq compilation-scroll-output t)

;; Always show colors in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Always show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Wrap long lines
(set 'truncate-partial-width-windows nil)

;; Show matching parentes
(show-paren-mode t)

;; Show column number next to line number
(column-number-mode t)

;; Use spaces for indentation
(set-default 'indent-tabs-mode nil)

;; Don't use GNU indentation mode for C/C++ files
(c-set-offset 'substatement-open 0)

;; Open .h files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Get rid of those '~' backup files
(setq make-backup-files nil)

;; No indentation of C++ namespaces
(c-set-offset 'innamespace 0)

;; Indent cases in switch statements
(c-set-offset 'case-label '+)

;; No extra indentation in C++ lambda statements
(c-set-offset 'inlambda 0)

;; Fix indentation of multiline C++ brace initializers
(c-set-offset 'brace-list-intro '+)

;; Slightly less indentation in shell scripts
(setq sh-basic-offset 2)
