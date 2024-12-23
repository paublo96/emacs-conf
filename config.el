;; Tested on emacs-28.2

;; ---------- Stock Emacs Options ----------

;; Default UI style
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(display-time-mode 1)
(setq column-number-mode t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
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

;; Load packages from $EMACS_PACKAGE_DIR
(setq custom-package-dir (getenv "EMACS_PACKAGE_DIR"))
(package-initialize)

;; Simplified package management
(eval-when-compile
  ;;(push (expand-file-name custom-package-dir "use-package") load-path)
  (add-to-list 'load-path (concat custom-package-dir "/use-package"))
  (require 'use-package))

;; ---------- Themes ----------

(add-to-list 'load-path (concat custom-package-dir "/doom-themes"))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t))

;; ---------- Programming Language Modes ---------

(add-to-list 'load-path (concat custom-package-dir "/rust-mode"))
(use-package rust-mode)

(add-to-list 'load-path (concat custom-package-dir "/cmake-mode"))
(use-package cmake-mode)

(add-to-list 'load-path (concat custom-package-dir "/yaml-mode"))
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; ---------- Completion ----------

;; eglot requires newer project version than builtin
(add-to-list 'load-path (concat custom-package-dir "/project"))
(add-to-list 'load-path (concat custom-package-dir "/external-completion"))
(add-to-list 'load-path (concat custom-package-dir "/jsonrpc"))
(add-to-list 'load-path (concat custom-package-dir "/eglot"))
;; LSP support
(use-package eglot
  :config
  (setq eglot-autoshutdown t)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'fortran-mode-hook 'eglot-ensure)
  (add-hook 'rust-mode-hook 'eglot-ensure))

(add-to-list 'load-path (concat custom-package-dir "/compat"))
(add-to-list 'load-path (concat custom-package-dir "/corfu"))
;; LSP completions overlay
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :config
  (global-corfu-mode))

(add-to-list 'load-path (concat custom-package-dir "/vertico"))
(add-to-list 'load-path (concat custom-package-dir "/vertico/extensions"))
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

(add-to-list 'load-path (concat custom-package-dir "/consult"))
;; Nice search/navigation commands
(use-package consult
  :bind(("C-c g" . consult-grep)
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

(add-to-list 'load-path (concat custom-package-dir "/yasnippet"))
;; Template snippet completions
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; ---------- Tools ----------

;; Requires compat (loaded previously)
(add-to-list 'load-path (concat custom-package-dir "/dash"))
(add-to-list 'load-path (concat custom-package-dir "/transient/lisp"))
(add-to-list 'load-path (concat custom-package-dir "/with-editor/lisp"))
(add-to-list 'load-path (concat custom-package-dir "/magit/lisp"))
(add-to-list 'load-path (concat custom-package-dir "/magit"))
;; Git support
(use-package magit
  :bind(("C-c g" . magit-status))
  :config
  (setq magit-push-current-set-remote-if-mising nil))

(add-to-list 'load-path (concat custom-package-dir "/hdf5-mode"))
;; Simple HDF5 file viewer
(use-package hdf5-mode)
