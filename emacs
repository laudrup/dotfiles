; -*- mode: Lisp;-*-

;; Setup packages
(require 'package)

;; Extra package repositories
(add-to-list 'package-archives
	           '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
;; Initialize the package system
(package-initialize)

;; The use-package package needs to be installed explicitly
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package all-the-icons
  :ensure t)

;; Load magit
(use-package magit
  :ensure t
  :bind
  ([f11] . magit)
  ("C-c b" . magit-blame)
  :config
  (setq smerge-command-prefix "\C-c"))

;; Load and use flycheck
(use-package flycheck
  :ensure t
  :bind
  ("C-c n" . flycheck-next-error)
  ("C-c p" . flycheck-previous-error))

(use-package spaceline
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

;; Load custom theme
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t))
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

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

;; Python virtual environment activation
(use-package auto-virtualenv
  :ensure t
  :init)

(use-package pyvenv
  :ensure t
  :config
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv))

;; Bitbake mode
(use-package bitbake
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.bb?\\'" . bitbake-mode)))

;; Powershell mode
(use-package powershell
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ps1?\\'" . powershell-mode)))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  ;; Disable mode line. Speeds up projectile over tramp
  (setq projectile-mode-line "Projectile")
  :bind
  ("C-c f" . projectile-ag))

;; Company
(use-package company
  :ensure t
  :config
  (use-package company-box
    :ensure t
    :config (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :hook (company-mode . company-box-mode)
  (global-company-mode)))

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (use-package lsp-ui
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
  :bind ("M-f" . lsp-execute-code-action)
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
  :commands lsp)

(defun my--tramp-send-command--workaround-stty-icanon-bug (conn-vec orig-command &rest args)
  "See: https://github.com/magit/magit/issues/4720"
  (let ((command
         (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
             "stty -icrnl"
           orig-command)))
    (append (list conn-vec command) args)))

(defun my--tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
  (apply #'my--tramp-send-command--workaround-stty-icanon-bug args))

(advice-add 'tramp-send-command :filter-args
            #'my--tramp-send-command--workaround-stty-icanon-bug--filter-args)

(setq tramp-controlmaster-options "-o ControlMaster=auto -o ControlPersiste=no")

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
  :ensure t)

;; Markdown mode
(use-package markdown-mode
  :ensure t)

;; JSON mode
(use-package json-mode
  :ensure t)

;; Groovy mode
(use-package groovy-mode
  :ensure t
  :config
  (setq groovy-indent-offset 2))

;; Ini mode for configuration files
(use-package ini-mode
  :ensure t)

;; Isortify for python imports
(use-package isortify
  :ensure t)

;; Use editorconfig if available
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Flyspell for text modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Get rid of annoying GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Get rid of annoying splash screen
(setq inhibit-splash-screen t)

;; Get rid of annoying bell
(setq ring-bell-function 'ignore)

;; Use spaces not tabs
(setq-default
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2)

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ensure UTF-8
(set-language-environment "UTF-8")

(add-to-list 'default-frame-alist
             '(font . "Aporetic Sans Mono-12"))

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

(defun ansi-colors-region ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

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

;; Open .h and .ipp files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

;; Open WiX files in XML mode
(add-to-list 'auto-mode-alist '("\\.wxs\\'" . nxml-mode))

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

;; No indentation of extern "C"
(c-set-offset 'inextern-lang 0)

;; Slightly less indentation in shell scripts
(setq sh-basic-offset 2)

;; Don't type yes or no at prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable dir-locals for remote buffers
(setq enable-remote-dir-locals t)

;; Transparent background
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Load/save customs elsewhere to avoid modifying this file
(setq custom-file "~/.config/emacs-custom.el")
(load custom-file)
