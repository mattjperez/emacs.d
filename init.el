;; TODOs
;; - dir-locals - not in this file somehow
;; - cape?
;; - supersave
;; - C-a C-a beginning of line beginning of statement fix
;; - treesit directory no-littering
;; - C++ treesit grammer version mismatch

;;
;; Very early setup, enough to ensure we can avoid littering even through
;; bringing up package/use-package/no-littering
;;
(defvar no-littering-etc-directory (expand-file-name ".cache/etc/" user-emacs-directory))
(defvar no-littering-var-directory (expand-file-name ".cache/var/" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa/" no-littering-var-directory))

;;
;; Top-level configuration for `package`, `use-package`, and `auto-compile`
;;
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archive-priorities '("melpa-stable" . 10) t)
(add-to-list 'package-archive-priorities '("melpa" . 1) t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")
(use-package use-package)

(use-package no-littering
  :demand
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (no-littering-theme-backups))

(use-package auto-compile
  :demand
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode))


(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)


;;
;; Use use-package for emacs configurations
;;
(use-package emacs
  :custom
  ;; Indispensable top level key bindings for macos
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super)

  ;; TAB cycle if only few candidates
  (setq completion-cycle-threshold 3)

  ;; Sideline custom.el per no-littering
  (custom-file (no-littering-expand-etc-file-name "custom.el"))

  ;; No need for the startup screen
  (inhibit-startup-screen t)

  ;; Make `TAB` smarter
  (tab-always-indent 'complete)

  ;; Bell-ring is annoying
  (ring-bell-function 'ignore)

  ;; Always add a trailing newline to files
  (require-final-newline t)

  ;; Configure buffer uniqueness
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator "|")

  :config
  ;; Toolbar/scrollbar is a waste of space
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Blinking cursor is silly
  (blink-cursor-mode -1)

  ;; Include more metadata in the modeline
  (line-number-mode +1)
  (column-number-mode +1)
  (size-indication-mode +1)

  ;; We want this for magit
  (global-unset-key (kbd "s-m"))

  ;; Don't require typing out `yes` or `no`; accept `y` or `n`.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Always start the initial frame large
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; Delete the selection on any keypress
  (delete-selection-mode +1)

  ;; If files change outside emacs, automatically reload them
  (global-auto-revert-mode +1)

  ;; global line numbers
  (global-display-line-numbers-mode +1))

  ;; Inconsolata-16 as the default
  ;; TODO: Can this go into the `solarized` use-pacakge below?
  ;; (add-to-list 'default-frame-alist '(font . "Inconsolata-16")))


;;
;; Get some theme in there, and other UI stuff
;;
(use-package gruvbox-theme
  :defer t
  :init
  (load-theme 'gruvbox-dark-medium t))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package diminish)


;;
;; Remember things with recentf, savehist, and saveplace
;;
(use-package recentf
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 1000)
  :config
  (recentf-mode +1))

(use-package savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)
  :config
  (savehist-mode +1))

(use-package saveplace
  :config
  (save-place-mode +1))


;;
;; Load MOVEC and which-key
;;
(use-package vertico
  :init
  (vertico-mode +1)
  (vertico-multiform-mode +1)
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  :custom
  (enable-recursive-minibuffers t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :after (projectile vertico)
  :custom
  (consult-narrow-key "<")
  (consult-project-function (lambda (_) (projectile-project-root)))
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
)

(use-package consult-dir
  :pin melpa  ;; stable is too old
  :after (vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :custom
  (consult-dir-project-list-function 'consult-dir-projectile-dirs)
  :init
  (defun my/consult-dir--tramp-container-hosts ()
      (cl-loop for (id name) in (tramp-container--completion-function tramp-docker-program)
           collect (format "/docker:%s" name)))

  (defvar my/consult-dir--source-tramp-container
    `(:name     "Docker"
      :narrow   ?d
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'my/consult-dir--tramp-container-hosts)
    "Docker candidate source for `consult-dir'.")

  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'my/consult-dir--source-tramp-container t))

(use-package consult-projectile
  :pin melpa
  :after (consult projectile)
  :bind
  (:map projectile-mode-map
        ("s-p f" . consult-projectile)))

(use-package marginalia
  :init
  (marginalia-mode +1))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C->" . embark-dwim)
   ("s-]" . embark-collect)
   ("s-}" . embark-export)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; From https://karthinks.com/software/avy-can-do-anything/
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
  :custom
  (embark-indicators '(
    embark-minimal-indicator  ; default is embark-mixed-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))
  (embark-prompter 'embark-completing-read-prompter)
  (embark-cycle-key ".")
  (embark-help-key "?"))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :diminish
  :config
  (which-key-mode +1))


;;
;; Completion - Let's Try Corfu
;;
(use-package corfu
  :custom
  (corfu-cycle t) ;; enable cycling for `corfu-next/previous`
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("<escape>" . corfu-reset)
        ("C-g" . corfu-quit)
        ("s-SPC" . corfu-insert-separator)
        ("C-p" . corfu-previous)
        ))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))


;;
;; Load magit, projectile, fly[check,spell], compile, etc. as key programming configs.
;;
(use-package magit
  ;; (setq magit-blame-echo-style 'headings)
  :bind (("s-m m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer)
         ("s-m b" . magit-blame)))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :custom
  (projectile-per-project-compilation-buffer t))

(use-package flycheck
  :diminish
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck
  :after (consult)
)

(use-package flyspell
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package flyspell-correct
  :after flyspell
  :defer t)

(use-package consult-flyspell
  :pin melpa
  :after (consult)
  :config
  (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package compile
  :custom
  (compilation-max-output-line-length nil)
  (compilation-skip-threshold 0))

(use-package smartparens
  :diminish smartparens-mode
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  :custom
  (sp-base-key-bindings 'sp))

(use-package eldoc
  :diminish eldoc-mode)


;;
;; Navigation in space and time (tramp, windows, undo, etc.)
;;
(use-package undo-tree
  :pin gnu
  :diminish
  :custom
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode +1))

(use-package ace-window
  :demand
  :init
  ;; From https://karthinks.com/software/emacs-window-management-almanac/#aw-select-the-completing-read-for-emacs-windows
  (defun my/ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command
invoked immediately after this command (ignoring reading from the
minibuffer). Creates a new window before displaying the
buffer. When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (defun my/ace-window-one-command ()
  (interactive)
  (let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((command (key-binding
                         (read-key-sequence
                          (format "Run in %s..." (buffer-name)))))
               (this-command command))
          (call-interactively command))))))
  :bind
  ([remap other-window] . ace-window)
  ("C-x 4 o" . 'my/ace-window-prefix)
  ("C-x O" . 'my/ace-window-one-command)
  :custom
  (switch-to-buffer-obey-display-actions t)
  (aw-dispatch-always t)
  )

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-show-ad-hoc-proxies t)
  (enable-remote-dir-locals t)
  (tramp-use-ssh-controlmaster-options nil)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package avy
  :custom
  (avy-enter-times-out nil)
  (avy-all-windows 'all-frames)
  :bind
  (("M-g c" . avy-goto-char-timer)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("M-s a" . avy-isearch)))


;;
;; General editing
;;
(use-package guru-mode
  :diminish
  :config
  (guru-global-mode +1))

(use-package volatile-highlights
  :diminish
  :config
  (volatile-highlights-mode +1))

(use-package anzu
  :diminish
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package whitespace
  :diminish
  :init
  ;; TODO: Why doesn't this seem to work when done with :hook before-save?
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-line-column nil)
  (whitespace-style '(face tabs empty trailing)))

(use-package wgrep)


;;
;; Programming modes, tree-sitter, LSP
;;
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package rust-mode
  :demand
  :pin melpa
  :custom
  (rust-mode-treesitter-derive t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

(use-package rustic
  :pin melpa
  :after (rust-mode))

(use-package go-mode
  :hook
  (go-ts-mode . (lambda () (whitespace-toggle-options '(tabs))))
  (go-ts-mode . (lambda () (setq tab-width 2)))
  (go-ts-mode . (lambda () (setq go-ts-mode-indent-offset 2))))

(use-package lsp-mode
  :pin melpa
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-completion-provider :none) ;; Corfu!
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.01)
  ;; Having the inlays on all the time is distracting. Instead, toggle them on
  ;; interactively with `M-x lsp-inline-hints-mode` when you want them, then make
  ;; them go away by running it again.
  ;; TODO: Add a toggle for this under the LSP command keymap.
  ;; (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (c-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c-or-c++-mode . lsp-deferred)
         (c-or-c++-ts-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (cmake-mode . lsp-deferred)
         (cmake-ts-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred))
  :commands lsp)

(use-package lsp-ui
  :pin melpa
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil))

(use-package yasnippet
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :pin melpa
  :after (consult yasnippet))


;;
;; Other configuration
;;
(use-package ereader
  :pin melpa)


;;
;; Load sidelined custom.el
;;
(when (file-exists-p custom-file)
  (load custom-file))


;;
;; Start the server
;;
(server-start)
