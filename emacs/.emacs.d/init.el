;; hide toolbars immediately upon emacs start
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;;;;;;;;;;;;;; Server/Emacs daemon stuff

(server-start)

;;;;;;;;;;;;;;; Helper functions

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
          user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  "Load FILE from current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))


;;;;;;;;;;;;;;; Macbook-specific configuration

;; Bunch of stuff better off in another file
(load-user-file "scripts/macbook.el")

;; Change to a better font on the macbook
(when (is-macbook)
  (setq default-frame-alist '((font . "Source Code Pro-14"))))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;; Packages

(setq use-package-always-ensure t)

(use-package general)

(use-package avy)

(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0))


(use-package counsel
  :init
  (setq counsel-find-file-at-point t))

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq isearch-forward t) ;; Required to search downward by default when using swiper & the `n` key...
  (setq evil-ex-search-persistent-highlight nil)
  (evil-mode t))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :init
  (evil-commentary-mode))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  ;; For some reason, having flycheck check syntax every new line in purescript-mode
  ;; is very slow -- but only on nixos, not the macbook!
  (unless (is-macbook)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))))

(use-package haskell-mode
  :defer t)

(use-package dante
  :defer t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  :config
  (add-to-list 'company-backends 'dante-company)
  (setq dante-repl-command-line '("stack" "repl"))
  :general
  (:states '(normal insert)
   :keymaps 'haskell-mode-map
   "C-c C-t" 'dante-type-at
   "C-c s" 'dante-auto-fix))

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t   ; extend searching to bookmarks
        ivy-height 20               ; set height of the ivy window
        ivy-count-format "(%d/%d) " ; count format, from the ivy help page
        ivy-use-selectable-prompt t
  )
  (ivy-mode 1)
  )

(use-package ivy-bibtex
  :defer t
  :commands 'ivy-bibtex
  :init
  (general-define-key
    :states '(normal insert)
    :keymaps '(org-mode-map latex-mode-map)
    "C-c C-b" 'ivy-bibtex)
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package magit
  :init
  (use-package evil-magit)
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package nix-mode
  :defer t)

(use-package nixos-options
  :ensure t)

(use-package company-nixos-options
  :defer t
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package nix-sandbox
  :defer t)

(use-package psc-ide
  :diminish psc-ide-mode
  :general
  (:states 'normal
   :keymaps 'psc-ide-mode-map
           "C-c s" 'psc-ide-flycheck-insert-suggestion)
  (:states '(normal insert)
   :keymaps 'psc-ide-mode-map
   "M-ö" 'psc-ide-goto-definition
   "s-ö" 'psc-ide-goto-definition)
  :init
  (add-hook 'purescript-mode-hook 'psc-ide-mode))

(use-package purescript-mode
  :diminish purescript-indent-mode
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package rust-mode
  :defer t
  :commands 'rust-mode
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  )

(use-package racer
  :defer t
  :general
  (:states '(normal insert)
   :keymaps 'rust-mode-map
   "M-ö" 'racer-find-definition))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :init
  (use-package evil-smartparens
    :diminish evil-smartparens-mode
    :init
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  (require 'smartparens-config))
  ;; :config
  ;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package swiper)

(use-package yaml-mode
  :defer t)

(use-package yasnippet
  :diminish 'yas-minor-mode
  :defer t
  :init
  (yas-global-mode 1)
  :general
  (:states 'insert
           "C-c o" 'company-yasnippet
           "C-c e" 'yas-expand))

(use-package zoom
  :diminish zoom-mode
  :init
  (setq zoom-size '(0.618 . 0.618)
        ;; zoom-ignored-major-modes '(dired-mode markdown-mode)
        ;; zoom-ignored-buffer-names '("zoom.el" "init.el")
        ;; zoom-ignored-buffer-name-regexps '("^*calc"))
        ;; zoom-ignore-predicates '(lambda () (> (count-lines (point-min) (point-max)) 20)))
	)
  (zoom-mode t))


;;;;;;;;;;;;; Keybindings


(defvar leader-key ",")

;; bind win+{h,j,k,l} to move between windows
(general-define-key
  "s-h" 'evil-window-left
  "s-j" 'evil-window-down
  "s-k" 'evil-window-up
  "s-l" 'evil-window-right)

; Swiper search with / in normal mode
(general-define-key
  :states 'normal
  "/" 'swiper)

;; replace default keybindings to use ivy & co
(general-define-key
  "C-s"     'swiper
  "M-x"     'counsel-M-x
  "<f1> f"  'counsel-describe-function
  "<f1> v"  'counsel-describe-variable
  "<f1> l"  'counsel-find-library
  "<f2> i"  'counsel-info-lookup-symbol
  "<f2> u"  'counsel-unicode-char
  "C-C C-r" 'ivy-resume)

;; <C-f> as prefix for finding files
(general-define-key
  :states 'normal
  :keymaps 'global
  "C-f C-f" 'counsel-find-file
  "C-f C-g" 'counsel-git
  "C-f C-j" 'counsel-file-jump
  "C-f C-r" 'counsel-rg)

(general-define-key
  :states 'normal
  :keymaps 'dired
  "r" 'revert-buffer)

;; Bindings for company-mode
(general-define-key
  :states 'insert
  :keymaps 'company-mode-map
  "C-j" 'company-select-next
  "C-k" 'company-select-previous)


;; Nicer bindings for moving thru & dispatching ivy actions
(general-define-key
  :keymaps 'ivy-minibuffer-map
  "C-j" 'ivy-next-line
  "C-k" 'ivy-previous-line
  "C-'" 'ivy-avy
  "C-c" 'ivy-dispatching-done)


;; Nicer Svorak-y bindings for x-ref, duplicated for linux & mac
(general-define-key
  :states '(normal insert)
  "M-ä" 'xref-pop-marker-stack
  "M-ö" 'xref-find-definitions
  "M-å" 'xref-find-references
  "s-ä" 'xref-pop-marker-stack
  "s-ö" 'xref-find-definitions
  "s-å" 'xref-find-references)


(general-define-key
  :prefix leader-key
  :states 'normal
  "e" 'flycheck-list-errors)

(general-define-key
  :prefix leader-key
  :states 'normal
  "gs" 'magit-status
  "gb" 'magit-blame)

;; fix keybindings for the flycheck error list
(general-evil-define-key 'normal 'flycheck-error-list-mode-map
  "j" 'flycheck-error-list-next-error
  "k" 'flycheck-error-list-previous-error
  "<return>" 'flycheck-error-list-goto-error)
;;;;;;;;;;;;; Settings

;; remove trailing whitespace when saving a buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; spaces instead of tabs and other default indent config
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)


;; from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1)		; delete excess backup versions silently
(setq version-control t)		; use version control
(setq vc-make-backup-files t)		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t)				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";; Begin") ; print a default message in the empty scratch buffer opened at startup


;;;;;;;;;;;;; Org-mode stuff

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-cite)
    (default       . bibtex-completion-format-citation-cite)))


(use-package org
  :config
  (setq org-src-fontify-natively t))

(use-package ox-latex
  :defer t
  :after org
  :config
  (setq org-latex-listings 'minted
        org-latex-packages-alist '(("" "minted")))

  (add-to-list 'org-latex-minted-langs
               '(purescript "haskell"))
  (add-to-list 'org-latex-minted-langs
               '(javascript "javascript"))
  (add-to-list 'org-latex-minted-langs
               '(haskell "haskell")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)
   (emacs-lisp . t)
   ))


;;;;;;;;;;;;; Functions etc.

(defun sudo-edit (&optional arg)
  "Edit currently visited file as super user.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))



;;;;;;;;;;;;; Themes

(use-package zenburn-theme
  :defer t)
  ;; :config
  ;; (load-theme 'zenburn t t))

(use-package leuven-theme
  :defer t)
  ;; :config
  ;; (load-theme 'leuven t t))

(use-package material-theme
  :init
  (load-theme 'material t))
  ;; :config
  ;; (load-theme 'material t t)
  ;; (load-theme 'material-light t t))

;; (setq dark-theme 'material)
;; (setq light-theme 'leuven)

;; (setq current-theme dark-theme)

;; (defun toggle-theme ()
;;   "Toggle between dark and light themes, as defined by global variables
;; `dark-theme` and `light-theme`."
;;   (interactive)
;;   (disable-theme current-theme)
;;   (if
;;     (eq current-theme dark-theme)
;;     (setq current-theme light-theme)
;;     (setq current-theme dark-theme))
;;   (load-theme current-theme t))

;; (load-theme current-theme t)


;;;;;;;;;;;;; Customizer
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet dante ivy-bibtex exec-path-from-shell zoom zenburn-theme yaml-mode use-package rust-mode rainbow-delimiters purescript-mode psc-ide nixos-options nix-sandbox nix-mode material-theme leuven-theme haskell-mode general evil-surround evil-smartparens evil-magit evil-commentary counsel avy)))
 '(safe-local-variable-values
   (quote
    ((bibtex-completion-cite-prompt-for-optional-arguments)
     (bibtex-completion-bibliography . "./bibliography.bib")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
