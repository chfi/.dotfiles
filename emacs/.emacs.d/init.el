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
(setq jupiter-nix-hostname "jupiter-nix")
(setq macbook-hostname "Christians-MacBook-Air.local")

(load-user-file "scripts/macbook.el")

;; Can use (when (is-macbook) ..) and (text-scale-set `mul`) to change font size on laptop
;; (setq default-frame-alist '((font . "Source Code Pro-12")))

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130 :weight 'normal)


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

(use-package default-text-scale
  :init
  (default-text-scale-mode))

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-d-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq isearch-forward t) ;; Required to search downward by default when using swiper & the `n` key...
  (setq evil-ex-search-persistent-highlight nil)
  (evil-mode t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (use-package evil-org
    :ensure t
    :after org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

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

(use-package hydra
  :ensure t
  :defer nil)

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t   ; extend searching to bookmarks
        ivy-height 20               ; set height of the ivy window
        ivy-count-format "(%d/%d) " ; count format, from the ivy help page
        ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package ivy-bibtex
  :defer t
  :commands 'ivy-bibtex
  :init
  (general-define-key
    :states '(normal insert)
    :keymaps '(org-mode-map latex-mode-map)
    "C-c C-b" 'ivy-bibtex)
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package jq-mode
  :defer t
  :commands 'jq-mode
  :mode (("\\.jq\\'" . jq-mode)))

(use-package magit
  :init
  (use-package evil-magit)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


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

;; (use-package smartparens
;;   :init
;;   (use-package evil-smartparens
;;     :diminish evil-smartparens-mode
;;     :init
;;     (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
;;   (require 'smartparens-config))


;; (use-package lispy
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;;   (use-package lispyville
;;     :init
;;     (add-hook 'lispymode-hook 'lispyville-mode)))


(use-package swiper
  :ensure t)

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



(use-package nand2tetris
  :defer t
  :config
  (use-package nand2tetris-assembler
    :defer t)
  (use-package company-nand2tetris
    :defer t))


;;;;;;;;;;;;; Keybindings


(defvar leader-key ",")

;; (defun i3-windmove (dir &optional count)
;;   "Change EMACS focus to the next window in direction DIR.
;; If there is no window there, signal i3 to change window focus.
;; COUNT is ignored currently."
;;   (when (member dir '(left right up down))
;;     (condition-case nil
;;         (windmove-do-window-select dir)
;;       (user-error
;;        (call-process-shell-command
;;         (concat "i3-msg 'focus " (symbol-name dir) "'")
;;         nil 0 nil)))))

;; (evil-define-command i3-window-left (count)
;;   (interactive "p")
;;   (i3-windmove 'left))

;; (evil-define-command i3-window-right (count)
;;   (interactive "p")
;;   (i3-windmove 'right))

;; (evil-define-command i3-window-up (count)
;;   (interactive "p")
;;   (i3-windmove 'up))

;; (evil-define-command i3-window-down (count)
;;   (interactive "p")
;;   (i3-windmove 'down))


(general-define-key
  "s-h" 'evil-window-left
  "s-l" 'evil-window-right
  "s-j" 'evil-window-down
  "s-k" 'evil-window-up)

;; (if (is-macbook)
;;   (general-define-key
;;     "s-h" 'evil-window-left
;;     "s-l" 'evil-window-right
;;     "s-j" 'evil-window-down
;;     "s-k" 'evil-window-up)
;;   (general-define-key
;;     "s-h" 'i3-window-left
;;     "s-l" 'i3-window-right
;;     "s-j" 'i3-window-down
;;     "s-k" 'i3-window-up))

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


(general-define-key
  :states 'normal
  "SPC" 'evil-scroll-down
  "S-SPC" 'evil-scroll-up)


;; Better org-mode bindings, compat w/ i3 etc.
;; (general-define-key
;;   :states '(normal insert)
;;   :keymaps 'org-mode-map
;;   "s-<return>" 'org-insert-heading
;;   "C-c C-q" 'counsel-org-tag
;;   )


(general-define-key
  :states '(normal insert)
  "C-c a" 'org-agenda)

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


;; Set org-directory to SyncThing directory (maybe better to symlink? idk)
(setq org-directory "~/Sync/org/")
(setq initial-buffer-choice (concat org-directory "journal/templates/reflection-morning.org"))


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
  ;;       org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)
   (emacs-lisp . t)
   ))

(setq org-capture-templates
      '(;; other entries
        ("t" "Todo" entry (file+headline "~/Sync/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal entry" entry
         (file+datetree+prompt "~/Sync/org/journal.org")
         ;; "%K - %a\n%i\n%?\n")))
         "* %U\n  %i\n  %a\n %?\n")))

(setq org-startup-indented t)
(setq org-tags-column 1)

;; file name for today's file
;; (format-time-string "%Y-%m-%d-entry.org" (current-time))


(setq journal-dir "~/Sync/org/journal/")
(setq journal-template-dir (concat journal-dir "templates/"))
(setq date-format "%Y-%m-%d/")
(defun date-string ()
  "Return the current date as a string."
  (format-time-string date-format (current-time)))

(defun journal-today-file (file)
  "(Create and) open FILE in the journal directory corresponding to today's date."
  (find-file (concat journal-dir (date-string) file)))

(defun journal-today-file-template (file template)
  "(Create and) open FILE in the journal directory corresponding to today's date,
filling it with the contents of TEMPLATE if it does not exist."
  (let ((filename (concat journal-dir (date-string) file)))
    (if (file-exists-p filename)
        (find-file filename)
      (progn
        (copy-file template filename)
        (find-file filename)))))

(defun journal-today-morning (&optional arg)
  "Open the file for today's morning reflection."
  (interactive "p")
  (journal-today-file-template "reflection-morning.org" (concat journal-template-dir "reflection-morning.org")))


(defun journal-today-evening (&optional arg)
  "Open the file for today's evening reflection."
  (interactive "p")
  (journal-today-file-template "reflection-evening.org" (concat journal-template-dir "reflection-evening.org")))

(defun journal-today-schedule (&optional arg)
  "Open the file for today's block schedule."
  (interactive "p")
  (journal-today-file-template "block-schedule.org" (concat journal-template-dir "block-schedule.org")))

(defun journal-today-main (&optional arg)
  "Open the file for today's main journal entries."
  (interactive "p")
  (journal-today-file "journal.org"))



(add-hook 'evil-org-mode-hook
  (lambda ()

    (general-evil-define-key 'normal 'evil-org-mode-map
      "-" 'org-ctrl-c-minus
      "|" 'org-table-goto-column)

    (general-define-key
     :states '(normal insert)
     "C-c c" 'org-capture
     "C-c C-q" 'counsel-org-tag)

    (general-define-key
     :prefix leader-key
     :states 'normal
     :keymaps 'evil-org-mode-map
     "." 'hydra-org-state/body
     "t" 'org-todo
     "T" 'org-show-todo-tree
     "v" 'org-mark-element
     "a" 'org-agenda
     "c" 'org-archive-subtree
     "l" 'evil-org-open-links
     "C" 'org-resolve-clocks)

    (defhydra hydra-org-state (:hint nil)
      "
^_i_, _I_: Cycle fold states

^_h_, _j_, _k_, _l_: Navigate elements

^_n_, _p_: Next/Previous link
^_o_: Open link at point

^_H_, _J_, _K_, _L_: Shift TODO
^_t_: Cycle TODO

^_N_, _P_: Next/Previous code block
"
      ;; basic navigation
      ("i" org-cycle)
      ("I" org-shifttab)
      ("h" org-up-element )
      ("l" org-down-element )
      ("j" org-forward-element)
      ("k" org-backward-element)
      ;; navigating links
      ("n" org-next-link)
      ("p" org-previous-link)
      ("o" org-open-at-point)
      ;; navigation blocks
      ("N" org-next-block)
      ("P" org-previous-block)
      ;; updates
      ("." org-ctrl-c-ctrl-c)
      ("*" org-ctrl-c-star)
      ("-" org-ctrl-c-minus)
      ;; change todo state
      ("H" org-shiftleft)
      ("L" org-shiftright)
      ("J" org-shiftdown)
      ("K" org-shiftup)
      ("t" org-todo))
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
  :defer t
  :init
  (load-theme 'zenburn t t))


(use-package leuven-theme
  :defer t)
  ;; :config
  ;; (load-theme 'leuven t t))

(use-package material-theme
  :defer t)

(enable-theme 'zenburn)
  ;; :init
  ;; (load-theme 'material t))
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
