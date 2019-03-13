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

;; RTags C/C++ indexing
(use-package rtags
  :ensure t
  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-find-references-at-point))
  :custom
  (rtags-display-result-backend 'helm)
  :config
  (use-package flycheck-rtags
    :ensure t)
  (use-package helm-rtags
    :ensure t)
  (use-package company-rtags
    :ensure t
    :config
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends))
  (defun rtags-index-directory ()
    (interactive)
    (rtags-start-process-unless-running)
    (let ((dir (read-directory-name "Directory with compilation database: ")))
      (rtags-call-rc "-J" dir)))
  :hook
  (c++-mode . rtags-start-process-unless-running))

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

;; Start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ensure UTF-8
(set-language-environment "UTF-8")

;; Set the default font
(set-frame-font "Hack-10" nil t)

;; Custom global key bindings
(global-set-key [f8] 'goto-line)
(global-set-key [f9] 'compile)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

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

;; No extra indentation in lambda statements
(c-set-offset 'inlambda 0)

;; Slightly less indentation in shell scripts
(setq sh-basic-offset 2)
