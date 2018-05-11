;; hide toolbars immediately upon emacs start
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Use straight.el for package management
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(server-start)

;; ;;;;;;;;;;;;;;; Helper functions

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


;; ;;;;;;;;;;;;;;; Macbook-specific configuration

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

;; ;;;;;;;;;;;;;;; Packages


(use-package diminish)

(use-package general)

(use-package avy
  :config
  (setq-default avy-keys '(97 111 101 117 105 104 116 110 115)))

(use-package ace-window
  :general
  (:keymaps 'override
   "M-n" 'ace-window))

(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0)
  :general
  (:states '(normal insert)
   "C-SPC" 'company-complete))


(use-package counsel
  :init
  (setq-default counsel-find-file-at-point t))

(use-package default-text-scale
  :init
  (default-text-scale-mode))

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq-default evil-want-C-u-scroll nil)
  (setq-default evil-want-C-d-scroll nil)
  (setq-default evil-want-C-i-jump nil)
  (setq-default undo-tree-visualizer-timestamps t)
  (setq-default undo-tree-visualizer-diff t)
  (setq-default isearch-forward t) ;; Required to search downward by default when using swiper & the `n` key...
  (setq-default evil-ex-search-persistent-highlight nil)
  (evil-mode t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  :config
  (general-define-key
    :states 'normal
    "SPC" 'evil-scroll-down
    "S-SPC" 'evil-scroll-up))

(use-package evil-escape
  :diminish evil-escape-mode
  :after evil
  :config
  (setq-default evil-escape-key-sequence "uö")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode))


(use-package evil-org
  :after evil
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "F" 'org-agenda-follow-mode))

(use-package evil-commentary
  :diminish evil-commentary-mode
  :init
  (evil-commentary-mode))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1)
  :general
  (:states 'visual
   :keymaps 'override
   "s" 'evil-surround-region))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  ;; For some reason, having flycheck check syntax every new line in purescript-mode
  ;; is very slow -- but only on nixos, not the macbook!
  (unless (is-macbook)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
    (setq flycheck-idle-change-delay 4.0)))

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

(use-package hydra)

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t   ; extend searching to bookmarks
        ivy-height 20               ; set height of the ivy window
        ivy-count-format "(%d/%d) " ; count format, from the ivy help page
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil)
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

;; (use-package jq-mode
;;   :defer t
;;   :commands 'jq-mode
;;   :mode (("\\.jq\\'" . jq-mode)))

(use-package magit
  :init
  (use-package evil-magit)
  (setq magit-completing-read-function 'ivy-completing-read))

;; (use-package markdown-mode
;;   :ensure t
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "multimarkdown"))


(use-package nix-mode
  :defer t)

(use-package nixos-options
  :defer t)

(use-package nov
  :defer t
  :commands 'nov-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq-default nov-text-width 80))

(use-package company-nixos-options
  :defer t
  :after company
  :config
  (add-to-list 'company-backends 'company-nixos-options))

(use-package nix-sandbox
  :defer t)

(use-package psc-ide
  :defer t
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
  ;; :config
  ;; (setq psc-ide-editor-mode t))

(use-package purescript-mode
  :defer t
  :diminish purescript-indent-mode
  :init
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))


(use-package rust-mode
  :defer t
  :commands 'rust-mode
  :config
  (add-hook 'rust-mode-hook 'racer-mode))

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


(use-package swiper)

(use-package yaml-mode
  :defer t)

(use-package yasnippet
  :diminish 'yas-minor-mode
  :init
  (yas-global-mode 1)
  :general
  (:states 'insert
           "C-c o" 'company-yasnippet
           "C-c e" 'yas-expand))

(use-package zoom
  :diminish zoom-mode
  :init
  (setq-default zoom-size '(0.618 . 0.618))
  (zoom-mode t))

;; (use-package nand2tetris
;;   :defer t
;;   :config
;;   (use-package nand2tetris-assembler
;;     :defer t)
;;   (use-package company-nand2tetris
;;     :defer t))


;; ;;;;;;;;;;;;; Keybindings


(general-override-mode)
(defvar leader-key "ä") ;; `ä` on Svorak is roughly comparable to `,` on QWERTY, ergonomically



(general-define-key
 :prefix leader-key
 :states 'normal
 :keymaps 'global
 "SPC" 'jump-to-register
 "w" 'window-configuration-to-register
 "p" 'point-to-register
 "1" '(lambda (_) (interactive "p") (jump-to-register ?1))
 "2" '(lambda (_) (interactive "p") (jump-to-register ?2))
 "3" '(lambda (_) (interactive "p") (jump-to-register ?3))
 "4" '(lambda (_) (interactive "p") (jump-to-register ?4))
 "h" '(lambda (_) (interactive "p") (jump-to-register ?h))
 "t" '(lambda (_) (interactive "p") (jump-to-register ?t))
 "n" '(lambda (_) (interactive "p") (jump-to-register ?n))
 "s" '(lambda (_) (interactive "p") (jump-to-register ?s))
 )



(general-define-key
  "s-h" 'evil-window-left
  "s-l" 'evil-window-right
  "s-j" 'evil-window-down
  "s-k" 'evil-window-up)


;; Swiper search with / in normal mode
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
  "<f2> u"  'counsel-unicode-char)


(general-define-key
  :states 'normal
  :keymaps 'override
  "Å" 'pop-global-mark
  "R" 'ivy-resume ;; i have never, ever, used `evil-replace-state`.
  "M-y" nil
  "-" nil
  "K" nil
  ;; "<f3> <f3>" 'org-agenda-list
  ;; "<f4>" 'org-agenda-list
  )



(general-define-key
 :states 'normal
 :keymaps 'global
 "s" 'ivy-switch-buffer
 "S" 'counsel-find-file)

;; ;; <C-f> as prefix for finding files
;; (general-define-key
;;   :states 'normal
;;   :keymaps 'global
;;   "C-f C-f" 'counsel-find-file
;;   "C-f C-g" 'counsel-git
;;   "C-f C-j" 'counsel-file-jump
;;   "C-f C-r" 'counsel-rg)

(general-define-key
  :states 'normal
  :keymaps 'dired
  "g r" 'revert-buffer)

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
  "M-." 'ivy-avy
  "C-SPC" 'ivy-avy
  "<backtab>" 'ivy-avy
  "C-s" 'ivy-dispatching-done)


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


;; Better org-mode bindings, compat w/ i3 etc.
;; (general-define-key
;;   :states '(normal insert)
;;   :keymaps 'org-mode-map
;;   "s-<return>" 'org-insert-heading
;;   "C-c C-q" 'counsel-org-tag
;;   )


(general-define-key
  :states '(normal insert)
  :keymaps 'global
  "C-c a" 'org-agenda)

;; ;;;;;;;;;;;;; Settings

;; remove trailing whitespace when saving a buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; enable abbrev-mode
(add-hook 'prog-mode-hook 'abbrev-mode)

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
(setq inhibit-startup-screen t)	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";; Begin") ; print a default message in the empty scratch buffer opened at startup

(setq auto-save-default nil) ; disable auto-save

(setq register-preview-delay 0)

(setq org-directory "~/Sync/org/")
(add-hook 'after-init-hook 'org-agenda-list)

;; ;;;;;;;;;;;;; Journal management


(setq journal-dir "~/Sync/org/journal/")
(setq journal-template-dir (concat journal-dir "templates/"))
(setq date-format "%Y-%m-%d")
(defun date-string ()
  "Return the current date as a string."
  (format-time-string date-format (current-time)))

(setq time-of-day-format "%H:%M")
(defun time-of-day-string ()
  "Return the current time of day as a string."
  (format-time-string time-of-day-format (current-time)))

(defun cf/journal-today-get-dir ()
  "Return the directory for today's journal entry, creating it if it does not exist."
  (let ((today-dir (concat journal-dir (date-string) "/")))
    (unless (file-exists-p today-dir)
      (make-directory today-dir t))
    today-dir))

;; (defun open-tasks (&optional arg)
;;   "Open my tasks list."
;;   (interactive "p")
;;   (find-file (concat org-directory "todo.org")))

;; (defun journal-today-file (file)
;;   "(Create and) open FILE in the journal directory corresponding to today's date."
;;   (find-file (concat (cf/journal-today-get-dir) file)))

;; (defun journal-today-file-template (file template)
;;   "(Create and) open FILE in the journal directory corresponding to today's date,
;; filling it with the contents of TEMPLATE if it does not exist."
;;   (let* ((today-dir (cf/journal-today-get-dir))
;;          (filename (concat today-dir file)))
;;     (if (file-exists-p filename)
;;         (find-file filename)
;;       (progn
;;         (copy-file template filename)
;;         (find-file filename)))))

;; (defun journal-today-morning (&optional arg)
;;   "Open the file for today's morning reflection."
;;   (interactive "p")
;;   (journal-today-file-template
;;    "reflection-morning.org"
;;    (concat journal-template-dir "reflection-morning.org")))


;; (defun journal-today-evening (&optional arg)
;;   "Open the file for today's evening reflection."
;;   (interactive "p")
;;   (journal-today-file-template
;;    "reflection-evening.org"
;;    (concat journal-template-dir "reflection-evening.org")))

;; (defun journal-today-schedule (&optional arg)
;;   "Open the file for today's block schedule."
;;   (interactive "p")
;;   (journal-today-file-template
;;    "block-schedule.org"
;;    (concat journal-template-dir "block-schedule.org")))

;; (defun journal-today-main (&optional arg)
;;   "Open the file for today's main journal entries."
;;   (interactive "p")
;;   (journal-today-file "journal.org"))



;;;;;;;;;;;;; Org-mode stuff

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-cite)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-cite)
    (default       . bibtex-completion-format-citation-cite)))


(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
    "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :config
  (require 'cl) ;; without this, using shift+up/down to modify timestamp minutes crashes
  (setq org-src-fontify-natively t)
  (add-to-list 'org-agenda-files org-directory)
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  (setq org-startup-indented t)
  (setq org-tags-column 1)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-indirect-buffer-display 'current-window)

  (setq org-tags-exclude-from-inheritance '("reading_now"))

  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "|" "DONE")
          (sequence "WAITING" "HOLD" "|" "CANCELLED")))

  (setq org-todo-keyword-faces
        '(("NEXT" :foreground "white" :background "gray33" :weight bold)
          ("WAITING" :foreground "lightgray" :weight bold)
          ("HOLD" :foreground "darkgray" :weight bold)
          ("CANCELLED"  :foreground "lightgray" :background "DarkRed" :weight bold)))

  (setq org-agenda-window-setup 'other-window)
  (setq org-agenda-restore-windows-after-quit t)


  ;; for easy templates after org 9.1
  (require 'org-tempo)

  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key "christian@chfi.se")

  (require 'org-habit)

  ;; decrypt :crypt: entries with C-c C-c when point in entry body
  (add-hook 'org-ctrl-c-ctrl-c-final-hook
            (lambda ()
              (when (and (not (org-at-heading-or-item-p))
                         (org-at-encrypted-entry-p))
                (org-decrypt-entry))))

  ;; for capturing from firefox etc.
  (require 'org-protocol)


  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (emacs-lisp . t))))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Sync/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")

        ("j" "Journal entry" entry
         (file (lambda () (concat (cf/journal-today-get-dir) "journal.org")))
         "* %(time-of-day-string)
** Mood
| Mood | Before % | After % |
|-
| %?     |          |         |\n
** Entry \n")

        ("l" "Add entry to library.org")

        ("la" "Add scientific publication with DOI" entry
         (file+olp (lambda () (concat org-directory "library/library.org")) "Articles" "Uncategorized")
         "* %^{Title}
:PROPERTIES:
:doi:  %^{DOI}
:year: %^{year}
:END:
%?\n")

        ("lb" "Add book" entry
         (file+olp (lambda () (concat org-directory "library/library.org")) "Books" "Uncategorized")
         "* %^{Title}
:PROPERTIES:
:author: %^{author}
:year: %^{year}
:END:
%?\n")


        ("a" "Activity monitoring and logs")

        ("an" "Next activity" entry
         (file+headline (lambda () (concat (cf/journal-today-get-dir) "journal.org")) "Monitoring")
         "* %(time-of-day-string) [/]

** Evaluation
*** Have you been mindful? Are you now?
If not, meditate for five minutes. Focus on your body, or your breathing, or your sensory inputs.
- [ ] I have been mindful
- [ ] I am now mindful

**** Is it easy for you to be with discomfort, and not be controlled by it?
- [ ] Yes
**** Is it easy for you to concentrate?
- [ ] Yes
**** Is it easy for you to dismiss distractions and return to focus?
- [ ] Yes

*** Have you used the last 30 minutes as planned?
- [ ] Yes

**** If not, have you been distracted? By what?

**** Why did you get distracted? What emotions and cognitions led you?

*** Have you blocked distracting sites, apps, etc.?
- [ ] Yes

*** Have you put away other distractions, such as books?
- [ ] Yes

*** Have you set a timer for the next period?
- [ ] Yes

*Hours of work remaining today: %?*

** What are you going to be doing the next 30 minutes?\n
** How confident are you in that you will follow this plan?
\n\n")

        ("al" "Activity log" entry
         (file (lambda () (concat (cf/journal-today-get-dir) "journal.org")))
         "* %(time-of-day-string)
** What have you been doing the past hour?
%?\n
** How aware are you? Have you been mindful of your experiences?\n
** What are you doing now, and why?\n
** What *should* you be doing now?\n
** Will you do what you should, now? If not, why?
- [ ] I've enabled OFFTIME, Leechblock, and killed irrelevant buffers and apps.
\n\n")


        ("p" "Protocol" entry
         (file+headline (lambda () (concat org-directory "notes.org")) "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")

        ("L" "Protocol Link" entry
         (file+headline (lambda () (concat org-directory "notes.org")) "Inbox")
         "* %? [[%:link][%:description]] \nCaptured On: %U")
        ))




(add-hook 'evil-org-mode-hook
  (lambda ()

    (general-evil-define-key 'normal 'evil-org-mode-map
      "-" 'org-ctrl-c-minus
      "|" 'org-table-goto-column)

    (general-evil-define-key 'normal 'evil-org-mode-map
      "s-H" 'org-shiftmetaleft
      "s-J" 'org-shiftmetadown
      "s-K" 'org-shiftmetaup
      "s-L" 'org-shiftmetaright)

    (general-evil-define-key '(normal insert) 'evil-org-mode-map
     "C-c C-q" 'counsel-org-tag)

    (general-define-key
     :states '(normal insert)
     "C-c c" 'org-capture
     "C-c l" 'org-store-link)

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

^_H_, _L_: Shift TODO
^_J_, _K_: Move subtree up/down
^_t_: Cycle TODO

^_N_, _P_: Next/Previous code block
"
      ;; basic navigation
      ("i" org-cycle)
      ("I" org-shifttab)
      ("h" org-up-element)
      ("l" org-down-element)
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
      ("J" org-move-subtree-down)
      ("K" org-move-subtree-up)
      ("t" org-todo))
    ))



;; ;;;;;;;;;;;;; Functions etc.

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



;; ;;;;;;;;;;;;; Themes

(use-package zenburn-theme
  :init
;; zenburn-default-colors-alist
  (load-theme 'zenburn t t))


(use-package material-theme
  :init
  (load-theme 'material t t)
  (load-theme 'material-light t t))


(enable-theme 'zenburn)


;;;;;;;;;;;;; Customizer
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(avy-keys (quote (97 111 101 117 105 104 116 110 115)))
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("a1e99cb36d6235abbe426a0a96fc26c006306f6b9d2a64c2435363350a987b4c" default)))
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#1c1f26")
 '(js-indent-level 2)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/Sync/org/notes.org" "~/Sync/org/genome-browser.org" "~/Sync/org/health.org" "~/Sync/org/reading/books/Mad_Science.org" "~/Sync/org/reading.org" "~/Sync/org/thesis.org" "~/Sync/org/common.org" "~/Sync/org/journal/2018-04-14/block-schedule.org" "~/Sync/org/journal/2018-04-13/block-schedule.org" "~/Sync/org/groceries.org" "~/Sync/org/todo.org")))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values
   (quote
    ((eval let nil
           (org-babel-goto-named-src-block "setup-export-class")
           (org-babel-execute-src-block))
     (bibtex-completion-cite-prompt-for-optional-arguments)
     (bibtex-completion-bibliography . "./bibliography.bib"))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
