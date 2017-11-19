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

;;;;;;;;;;;;;;; Helper functions

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
          user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


;;;;;;;;;;;;;;; Macbook-specific configuration

;; Bunch of stuff better off in another file
(load-user-file "scripts/macbook.el")

;; Change to a better font on the macbook
(on-macbook
 (setq default-frame-alist '((font . "Source Code Pro-14"))))


;;;;;;;;;;;;;;; Packages


(use-package general
  :ensure t)

(use-package avy
  :ensure t)

;; (use-package company
;;   :ensure t)

(use-package counsel
  :ensure t
  :config
  (setq counsel-find-file-at-point t))


(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
)

(use-package evil-magit
  :ensure t)

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'purescript-mode 'flycheck-mode))

(use-package haskell-mode
  :ensure t
  :defer t
)

(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
)

(use-package ivy-bibtex
  :ensure t)

(use-package lispy
  :ensure t)

(use-package magit
  :ensure t)

(use-package nix-mode
  :ensure t
  :defer t)

(use-package nixos-options
  :ensure t
  :defer t)

(use-package nix-sandbox
  :ensure t
  :defer t)

(use-package psc-ide
  :ensure t
  :defer t
  :config
  (add-hook 'purescript-mode-hook 'psc-ide-mode)
  :defer t)

(use-package purescript-mode
  :ensure t
  :defer t
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package rust-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package swiper
  :ensure t)

(use-package yaml-mode
  :ensure t
  :defer t)



;;;;;;;;;;;;; Settings



;; bind win+{h,j,k,l} to move between windows
(general-define-key
  ;; :states '(insert normal emacs help)
  "s-h" 'evil-window-left
  "s-j" 'evil-window-down
  "s-k" 'evil-window-up
  "s-l" 'evil-window-right)


; Swiper search with / in normal mode
(general-define-key
  :states 'normal
  "/" 'swiper)


(general-define-key
  ;; replace default keybindings
  "C-s"     'swiper       ; search in current buffer with swiper
  "M-x"     'counsel-M-x  ; replace M-x with ivy
  "C-x C-f" 'counsel-find-file
  "<f1> f"  'counsel-describe-function
  "<f1> v"  'counsel-describe-variable
  "<f1> l"  'counsel-find-library
  "<f2> i"  'counsel-info-lookup-symbol
  "<f2> u"  'counsel-unicode-char
  "C-c /"   'counsel-rg
  "C-C C-r" 'ivy-resume)
  ;; "C-c l"   'counsel-locate)


(general-define-key
  :keymaps 'ivy-minibuffer-map
  "C-j" 'ivy-next-line
  "C-k" 'ivy-previous-line
  "C-'" 'ivy-avy
  "C-c" 'ivy-dispatching-done)


(general-define-key
  :keymaps '(org-mode-map latex-mode-map)
  "C-c C-c" 'ivy-bibtex)


(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-cite)
    (default       . bibtex-completion-format-citation-cite)))


;; remove trailing whitespace when saving a buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; spaces instead of tabs, hail satan
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)


;; from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";; Begin") ; print a default message in the empty scratch buffer opened at startup




;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;; (setq org-latex-listings 'minted)

;; (setq org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)
   ;; (purescript . t)
   ;; (javascript . t)
   ))
   ;; (latex . t)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (general psc-ide nixos-sandbox evil-commentary nix-sandbox nixos-options nix-mode nix-emacs evil-visual-mark-mode)))
 '(safe-local-variable-values
   (quote
    ((bibtex-completion-cite-prompt-for-optional-arguments)
     (bibtex-completion-bibliography . "./bibliography.bib")
     (bibtex-completion-bibliography quote
                                     ("./bibliography.bib"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
