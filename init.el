;; Activate debugging.
;(setq debug-on-error t
;      debug-on-signal nil
;      debug-on-quit nil)

;; elpaca bootstrap program
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; enable elpaca support for use-package
;; use :ensure t to enable elpaca use-package support
;; use :ensure nil to prevent elpaca from loading built-ins
(elpaca elpaca-use-package
	(elpaca-use-package-mode))

;; set package download
(use-package no-littering
  :demand
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (no-littering-theme-backups))

;; set custom.el, init after elpaca
;(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(use-package emacs
  :ensure nil
  :init
  (setq user-full-name "Matthew J Perez"
      user-mail-address "matt@mperez.io")
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :hook (elpaca-after-init-hook . (lambda () (load custom-file 'noerror)))
  :custom
  (inhibit-startup-screen t)
  (tab-always-indent 'complete)
  (ring-bell-function 'ignore)
  (require-final-newline t)
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator "|")
  (auto-save-default nil)
  :config
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode -1)

  ;; modeline metadata
  (line-number-mode +1)
  (column-number-mode +1)
  (size-indication-mode +1)

  ;; reserve for magit
  (global-unset-key (kbd "s-m"))

  ;; accept y or n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; start initial frame large
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))

  ;; delete selection on any keypress
  (delete-selection-mode +1)

  ;; if files change outside emacs, auto-reload them
  (global-auto-revert-mode +1)

  (global-display-line-numbers-mode +1))

;;;
;;; Theme and UI
;;;
(use-package gruvbox-theme
  :ensure t
  :demand t
  :config
  (load-theme 'gruvbox-dark-medium t))
(use-package diminish
  :ensure t
  )
;; must install icons, all-the-icons-install-fonts
;(use-package all-the-icons
;  :if (display-graphic-p))
;(use-package all-the-icons-dired
;  :if (display-graphic-p)
;  )
(use-package solaire-mode
  :ensure t
  :functions solaire-global-mode
  :config
  (solaire-global-mode))

;;;
;;; Completion (General)
;;;
;; dynamic abbreviation
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; backend completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;;
;;; MiniBuffer Completion
;;;
;; frontend completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode +1)
  (vertico-multiform-mode +1)
  :defines vertico-multiform-categories
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  :custom
  (enable-recursive-minibuffers t))

;; backend completion functions
(use-package consult
  :ensure t
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
  :ensure t
  :after (vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :functions (tramp-container--completion-function my/consult-dir--tramp-container-hosts)
  :defines (tramp-docker-program consult-dir-sources)
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
  :after (consult projectile)
  :defines projectile-mode-map
  :bind
  (:map projectile-mode-map
        ("s-p f" . consult-projectile)))

;; annotations
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode +1))

;; actions on completion buffer
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C->" . embark-dwim)
   ("s-]" . embark-collect)
   ("s-}" . embark-export)
   ("C-h B" . embark-bindings))
  :defines avy-ring avy-dispatch-alist
  :functions ring-ref
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
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;
;;; Buffer Completion
;;;
;; frontend completion UI
(use-package corfu
  :ensure t
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
;; ;; backend completion functions, similar to consult
;; (use-package cape
;;   ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
;;   ;; Press C-c p ? to for help.
;;   :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
;;   ;; Alternatively bind Cape commands individually.
;;   ;; :bind (("C-c p d" . cape-dabbrev)
;;   ;;        ("C-c p h" . cape-history)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ...)
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   ;; (add-hook 'completion-at-point-functions #'cape-history)
;;   ;; ...
;;   )



;; General Editing
(use-package wgrep :ensure t)

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :diminish
  :custom
  (undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode +1))

;;;
;;; Navigation
;;;
;; move between windows with S-<arrow>
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-show-ad-hoc-proxies t)
  (enable-remote-dir-locals t)
  (tramp-use-ssh-controlmaster-options nil)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; in-view goto
(use-package avy
  :ensure t
  :custom
  (avy-enter-times-out nil)
  (avy-all-windows 'all-frames)
  :bind
  (("M-g c" . avy-goto-char-timer)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("M-s a" . avy-isearch)))


;;;
;;; Misc
;;;
(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;(use-package treemacs-magit
;  :after (treemacs magit)
;  :ensure t
;  :config
;  (treemacs-start-on-boot)
;  )

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map))
  :custom
  (projectile-per-project-compilation-buffer t))

(use-package rg
  :ensure t)

(use-package flycheck
  :ensure t
  :diminish
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package consult-flycheck
  :ensure t
  :after (consult)
)

(use-package flyspell
  :ensure nil
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :defer t)

(use-package consult-flyspell
  :ensure t
  :after (consult)
  :config
  (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

(use-package compile
  :ensure nil
  :custom
  (compilation-max-output-line-length nil)
  (compilation-skip-threshold 0))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  :custom
  (sp-base-key-bindings 'sp))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)


;;;
;;; Magit
;;;
(use-package transient
  :ensure (:branch "main" :wait t))
(use-package magit
  :ensure (:branch "main")
  :after (transient)
  ;; (setq magit-blame-echo-style 'headings)
  :bind (("s-m m" . magit-status)
         ("s-m j" . magit-dispatch)
         ("s-m k" . magit-file-dispatch)
         ("s-m l" . magit-log-buffer)
         ("s-m b" . magit-blame)))

;;;
;;; Treesitter, syntax highlighting
;;;
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :functions (treesit-auto-add-to-auto-mode-alist global-treesit-auto-mode)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;;;
;;; LSP
;;;
(use-package lsp-mode
  :ensure t
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
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-sideline-enable nil))

(use-package yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode)
  :functions yas-reload-all
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet))


;;
;; Other Programs
;;
(use-package ereader
  :ensure t)


