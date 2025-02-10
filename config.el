;; Tested on emacs-29.4

;; ---------- Stock Emacs Options ----------

;; Default UI style
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq column-number-mode t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq mode-line-percent-position nil)
;(set-face-attribute 'default nil :height 200)

;; Default behaviors
(setq confirm-kill-processes nil)
(setq tab-always-indent 'complete)
(setq ring-bell-function 'ignore)

;; Default programming style
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd")
(setq c-basic-offset 4)

;; Display current function in header line for programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Indent header line by line number column
            (header-line-indent-mode 1)
            (setq header-line-format
                  '(""
                    header-line-indent
                    (:propertize which-func-current face bold)))
            (setq which-func-unknown "")
            (which-function-mode t)
            ;; Remove which-function from mode line
            (setq mode-line-misc-info
                  (assq-delete-all
                   'which-function-mode mode-line-misc-info))))

;; Don't allow eldoc to display multiple lines unless prompted manually
(setq eldoc-echo-area-use-multiline-p nil)

;; Custom macros
(defun pm-copy-line-from-cursor ()
  (interactive)
  (save-excursion
    (kill-ring-save (point) (line-end-position)))
  (message "Copied line from cursor"))
(defun pm-copy-selection ()
  (interactive)
  (save-excursion (kill-ring-save (region-beginning) (region-end)))
  (message "Copied selection"))
(defun pm-copy-selection-or-line-from-cursor ()
  (interactive)
  (cond ((use-region-p) (pm-copy-selection))
        (t (pm-copy-line-from-cursor))))
(defun pm-multi-shell ()
  (interactive)
  (cond ((get-buffer "*shell*<1>") (progn (shell) (rename-uniquely)))
        (t (shell "*shell*<1>")))
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil)))
  (message "Launched new shell"))
(defun pm-multi-terminal ()
  (interactive)
  (cond ((get-buffer "*terminal*<1>") (progn (term "/usr/bin/bash")
                                             (rename-buffer "*terminal*")
                                             (rename-uniquely)))
        (t (progn (term "/usr/bin/bash")
                  (rename-buffer "*terminal*<1>"))))
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil)))
  (message "Launched new term"))

;; Custom keybindings
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(global-set-key (kbd "C-c s") 'pm-multi-shell)
(global-set-key (kbd "C-c t") 'pm-multi-terminal)
(global-set-key (kbd "C-c c") 'pm-copy-selection-or-line-from-cursor)
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)

;; C/C++ modes redefine <tab> key command which breaks corfu completion
(add-hook 'c++-mode-hook
 (lambda () (define-key c++-mode-map (kbd "<tab>") 'indent-for-tab-command)))
(add-hook 'c-mode-hook
 (lambda () (define-key c-mode-map (kbd "<tab>") 'indent-for-tab-command)))

;; Allow passing more keybindings through term mode
(add-hook
 'term-mode-hook
 (lambda ()
   (define-key term-raw-map (kbd "M-x")   'execute-extended-command)
   (define-key term-raw-map (kbd "C-v")   'scroll-up-command)
   (define-key term-raw-map (kbd "M-v")   'scroll-down-command)

   (define-key term-raw-map (kbd "C-c s") 'pm-multi-shell)
   (define-key term-raw-map (kbd "C-c t") 'pm-multi-terminal)
   (define-key term-raw-map (kbd "C-c c") 'pm-copy-selection-or-line-from-cursor)
   (define-key term-raw-map (kbd "C-c f") 'toggle-frame-fullscreen)
   (define-key term-raw-map (kbd "C-c k") 'kill-buffer-and-window)

   ;; Set two escape chars.
   (let (term-escape-char)
     (term-set-escape-char ?\C-x))
   (let (term-escape-char)
     (term-set-escape-char ?\C-c))))

;; ---------- Third-Party Packages ----------

(setq package-source-dir
      (concat (file-name-directory load-file-name) "packages/"))

(defun pm-install-local (package-name package-dir)
  (unless (package-installed-p package-name)
    (package-install-file (concat package-source-dir package-dir))))
(defun pm-install-local-min-ver (package-name package-dir min-ver)
  (unless (package-installed-p package-name min-ver)
    (package-install-file (concat package-source-dir package-dir))))

;; ---------- Themes ----------

;(add-to-list 'load-path (concat custom-package-dir "/doom-themes"))
(setq doom-themes-path (concat package-source-dir "doom-themes"))
(use-package doom-themes
  :load-path doom-themes-path
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t))

;; ---------- Programming Language Modes ---------

;(add-to-list 'load-path (concat custom-package-dir "/rust-mode"))
(pm-install-local 'rust-mode "rust-mode")
(use-package rust-mode)

;(add-to-list 'load-path (concat custom-package-dir "/cmake-mode"))
(setq cmake-mode-path (concat package-source-dir "cmake-mode"))
(use-package cmake-mode
  :load-path cmake-mode-path)

;(add-to-list 'load-path (concat custom-package-dir "/yaml-mode"))
(pm-install-local 'yaml-mode "yaml-mode")
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; ---------- Completion ----------

(pm-install-local-min-ver 'jsonrpc "jsonrpc-1.0.25.tar" '(1 0 25))
(pm-install-local-min-ver 'eldoc "eldoc-1.15.0.tar" '(1 15 0))
(pm-install-local-min-ver 'eglot "eglot-1.17.tar" '(1 17))
;; LSP support
(use-package eglot
  :config
  (setq eglot-autoshutdown t)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'fortran-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure))

(pm-install-local 'compat "compat")
(pm-install-local 'corfu "corfu")
;; LSP completions overlay
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :config
  (global-corfu-mode))

(pm-install-local 'vertico "vertico")
;; Minibuffer completions
(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-resize t)
  (setq vertico-cycle t)

  (setq completion-styles '(flex substring basic))
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (keymap-set vertico-map "C-l" 'backward-kill-word)
  (keymap-set vertico-map "C-;" 'vertico-insert)
  (keymap-set vertico-map "C-<return>" 'vertico-exit-input))
(use-package vertico-buffer
  :bind (("C-c b" . vertico-buffer-mode)))

;; Nice search/navigation commands
(pm-install-local 'consult "consult")
(use-package consult
  :bind(("C-c C-g" . consult-grep)
        ("M-g M-g" . consult-goto-line)))
(use-package consult-xref
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
(use-package consult-imenu
  :bind("C-c i" . consult-imenu))
(use-package consult-flymake
  :bind("C-c e" . consult-flymake))

(pm-install-local 'yasnippet "yasnippet")
;; Template snippet completions
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; ---------- Tools ----------

(pm-install-local 'dash "dash")
(pm-install-local 'with-editor "with-editor/lisp")
(setq magit-path (concat package-source-dir "../magit-install"))
(add-to-list 'load-path magit-path)
(load (concat magit-path "/magit-autoloads"))
(require 'magit)
;; Git support
(use-package magit
  :bind(("C-c g" . magit-status))
  :config
  (setq magit-push-current-set-remote-if-mising nil)
  (setq magit-section-initial-visibility-alist (quote ((untracked . hide)))))

;; Lisp IDE
(add-to-list 'load-path (concat package-source-dir "sly"))
(require 'sly-autoloads)
(use-package sly-autoloads
  :config
  (setq inferior-lisp-program "sbcl"))

(add-to-list 'load-path (concat package-source-dir "hdf5-mode"))
(require 'hdf5-mode)
;; Simple HDF5 file viewer
(use-package hdf5-mode)
