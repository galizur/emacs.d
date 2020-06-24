;;; package --- Summary
;;; Commentary:

;;; Code:

;;; Initialization
;; Package sources

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package delight :ensure t)
(use-package use-package-ensure-system-package :ensure t)

;;; Start-up
;; Authentication
(setq auth-sources '("~/.gnupg/shared/authinfo.gpg"
                     "~/.authinfo.gpg"
                     "~/.authinfo"
                     "~/.netrc"))

;; Better defaults
(setq-default
 ad-redefinition-action 'accept
 cursor-in-non-selected-windows t
 display-time-default-load-average nil
 fill-column 100
 help-window-select t
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-scratch-message ""
 kill-ring-max 128
 mark-ring-max 128
 load-prefer-newer t
 scroll-conservatively most-positive-fixnum
 select-enable-clipboard t
 tab-width 4
 use-package-always-ensure t
 user-full-name "Karolos Triantafyllou"
 user-mail-address "karolos.triantafyllou@gmail.com"
 vc-follow-symlinks t
 view-read-only t
 c-default-style "bsd"
 c-basic-offset 4)
(cd "~/")
(column-number-mode 1)
(global-display-line-numbers-mode)
(display-time-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode)
(set-default-coding-systems 'utf-8)
(show-paren-mode 1)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq auto-save-default nil)

;; Turn off mouse interface
(when window-system
  (menu-bar-mode -1)   ; Disable the menu bar
  (scroll-bar-mode -1) ; Disable the scroll bar
  (tool-bar-mode -1)   ; Disable the tool bar
  (tooltip-mode -1))   ; Disable the tooltips

;; Fonts
(set-face-attribute 'default nil :font "Source Code Pro Medium")
(set-fontset-font t 'latin "Noto Sans")

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package solaire-mode
  :custom (solair-mode-remap-fringe t)
  :config
  (solaire-mode-swap-bg)
  (solaire-global-mode +1))

;; Dired sort
(use-package dired-quick-sort
  :config
  (dired-quick-sort-setup))

;;; Languages
;; LSP
(use-package lsp-mode
  :hook ((c-mode c++-mode dart-mode java-mode python-mode xml-mode) . lsp)
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui)
(use-package company-lsp)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; Meson
(use-package meson-mode
  :hook ('meson-mode . company-mode))

;; C++
;; (use-package ccls
;;   :after projectile
;;   :ensure-system-package ccls
;;   :custom
;;   (ccls-args nil)
;;   (ccls-executable (executable-find "ccls"))
;;   (projectile-project-root-files-top-down-recurring
;;    (append '("compile_commands.json" ".ccls")
;;            projectile-project-root-files-top-down-recurring))
;;   :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

;; GLSL
(use-package glsl-mode)

;; Python
(use-package lsp-python-ms
  :defer 0.3
  :after lsp
  :custom
  (lsp-python-ms-dir
   (expand-file-name "~/.emacs.d/elisp/python-language-server/output/bin/Release/"))
  (lsp-python-ms-executable
   "~/.emacs.d/elisp/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

(use-package python
  :delight "π "
  :bind (("M-[" . python-nav-backward-block)
         ("M-]" . python-nav-forward-block)))

;; Emacs Lisp
(use-package elisp-mode :ensure nil :delight "ξ ")
;; Eldoc
(use-package eldoc
  :delight
  :hook (emacs-lisp-mode . eldoc-mode))

;; JSON
(use-package json-mode
  :delight "J "
  :mode "\\.json\\'"
  :hook (before-save . my/json-mode-before-save-hook)
  :preface
  (defun my/json-mode-before-save-hook ()
    (when (eq major-mode 'json-mode)
      (json-pretty-print-buffer)))

  (defun my/json-array-of-numbers-on-one-line (encode array)
    "Prints the arrays of nymbers in one line."
    (let* ((json-encoding-pretty-print
            (and json-encoding-pretty-print
                 (not (loop for x across array always (numberp x)))))
           (json-encoding-separator (if json-encoding-pretty-print "," ", ")))
      (funcall encode array)))
  :config (advice-add 'json-encode-array :around #'my/json-array-of-numbers-on-one-line))

;; Lua
(use-package lua-mode
  :delight "Λ "
  :mode "\\.lua\\'"
  :interpreter ("lua" . lua-mode))

;; Shell-script
(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; YAML
(use-package yaml-mode
  :delight "ψ "
  :mode "\\.yml\\'"
  :interpreter ("yml" . yml-mode))

;; HTML
(use-package emmet-mode
  :delight
  :hook (css-mode sgml-mode web-mode))

;; CSS-LESS-SCSS
(use-package css-mode
  :custom (css-indent-offset 2))
(use-package less-css-mode
  :mode "\\.less\\'")
(use-package scss-mode
  :mode "\\.scss\\'")

;; JavaScript
(use-package prettier-js
  :delight
  :custom (prettier-js-args '("--print-width" "100"
                              "--single-quite" "true"
                              "--trailing-coma" "all")))

(use-package js2-mode
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . prettier-js-mode))
  :mode "\\.js\\'"
  :custom (js-indent-level 2))

(use-package js2-refactor
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill)
              ("M-." . nil))
  :hook ((js2-mode . js2-refactor-mode)
         (js2-mode . (lambda ()
                       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  :config (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package xref-js2 :defer 5)

(use-package tern
  :ensure-system-package (tern . "npm install -g tern")
  :bind (("C-c C-c" . compile)
         :map tern-mode-keymap
         ("M-." . nil))
  :hook ((js2-mode . company-mode)
         (js2-mode . tern-mode)))

;;(use-package company-tern
;;  :after (company tern)
;;  :config (add-to-list 'company-backends 'company-tern))

;; PHP
(defun my/php-setup ()
  (web-mode)
  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset))

(use-package ac-php
  :after (company php-mode)
  :hook (php-mode . ac-php-mode)
  :custom (ac-sources '(ac-source-php))
  :config
  (ac-php-core-eldoc-setup)
  (auto-complete-mode t))

;; Web-mode
(use-package web-mode
  :delight "☸ "
  :hook ((css-mode web-mode) . rainbow-mode)
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.php$" . my/php-setup)
         ("\\.tsx\\'" . web-mode))
  :preface
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename matches the regexp."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup))))
  (add-to-list 'auto-mode-alist '("\\.php$" . my/php-setup))
  :custom
  (web-mode-attr-indent-offset 2)
  (web-mode-block-padding 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.js?\\'" . prettier-js-mode))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.ts?\\'" . prettier-js-mode))))
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

;; SQL
(use-package sql-indent
  :after (:any sql sql-interactive-mode)
  :delight sql-mode "Σ ")

;;; Advanced Configuration
;; Alert
(use-package alert
  :custom (alert-default-style 'libnotify))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; Auto-Completion
(use-package company
  :delight
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimim-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :delight
  :hook (company-mode . company-box-mode))

;; Buffers
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

(defvar *protected-buffers* '("*scratch*" "*Messages*")
  "Buffers that cannot be killed.")
(defun my/protected-buffers ()
  "Protects some buffers from being killed."
  (dolist (buffer *protected-buffers*)
    (with-current-buffer buffer
      (emacs-lock-mode 'kill))))

(add-hook 'after-init-hook #'my/protected-buffers)

;; Dired
(use-package dired
  :ensure nil
  :delight "Dired "
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(use-package dired-narrow
  :bind (("C-c C-n" . dired-narrow)
         ("C-c C-f" . dired-narrow-fuzzy)
         ("C-c C-r" . dired-narrow-regexp)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-cycle)
              ("<tab>" . dired-subtree-toggle)))

;; Hydra
(use-package hydra
  :bind (("C-c I" . hydra-image/body)
         ("C-c M" . hydra-merge/body)
         ("C-c T" . hydra-tool/body)
         ("C-c b" . hydra-btoggle/body)
         ("C-c e" . hydra-erc/body)
         ("C-c f" . hydra-flycheck/body)
         ("C-c g" . hydra-go-to-file/body)
         ("C-c m" . hydra-magit/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c q" . hydra-query/body)
         ("C-c s" . hydra-spelling/body)
         ("C-c t" . hydra-tex/body)
         ("C-c u" . hydra-upload/body)
         ("C-c w" . hydra-windows/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " "
              str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " "
              str)))

;; Hydra / BToggle
(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t))
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t))))


;; Hydra / ERC
(pretty-hydra-define hydra-erc
  (:hint nil :color teal :quit-key "q" :title (with-faicon "comments-o" "ERC" 1 -0.05))
  ("Action"
   (("b" my/erc-browse-last-url "browse last url")
    ("c" my/erc-start-or-switch "connect")
    ("d" erc-quit-server "disconnect")
    ("j" erc-join-channel "join")
    ("n" erc-channel-names "names")
    ("o" my/erc-get-ops "ops")
    ("u" my/erc-count-users "users")
    ("r" my/erc-reset-track-mode "reset track mode"))))

;; Hydra / Flycheck
(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

;; Hydra / Image
(pretty-hydra-define hydra-image
  (:hint nil :color pink :quit-key "q" :title (with-faicon "file-image-o" "Images" 1 -0.05))
  ("Action"
   (("r" image-rotate "rotate")
    ("s" image-save "save" :color teal))
   "Zoom"
   (("-" image-decrease-size "out")
    ("+" image-increase-size "in")
    ("=" image-transform-reset "reset"))))


;; Hydra / Magit
(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

;; Hydra / Merge
(pretty-hydra-define hydra-merge
  (:hint nil :color pink :quit-key "q" :title (with-alltheicon "git" "Merge" 1 -0.05))
  ("Move"
   (("n" smerge-next "next")
    ("p" smerge-prev "previous"))
   "Keep"
   (("RET" smerge-keep-current "current")
    ("a" smerge-keep-all "all")
    ("b" smerge-keep-base "base")
    ("l" smerge-keep-lower "lower")
    ("u" smerge-keep-upper "upper"))
   "Diff"
   (("<" smerge-diff-base-upper "upper/base")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "redefine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill current"))))


;; Hydra / Projectile
(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("D" projectile-dired "root")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache"))
   "Search"
   (("r" projectile-replace "replace")
    ("R" projectile-replace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

;; Hydra / Query
(pretty-hydra-define hydra-query
  (:hint nil :color teal :quit-key "q" :title (with-faicon "search" "Engine-Mode" 1 -0.05))
  ("Query"
   (("a" engine/search-amazon "amazon")
    ("d" engine/search-duckduckgo "duckduckgo")
    ("g" engine/search-github "github")
    ("i" engine/search-google-images "google images")
    ("m" engine/search-google-maps "google maps")
    ("s" engine/search-stack-overflow "stack overflow")
    ("w" engine/search-wikipedia "wikipedia")
    ("y" engine/search-youtube "youtube"))))

;; Hydra / Spelling
(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary)
     "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))

;; Hydra / TeX
(pretty-hydra-define hydra-tex
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "tex" "LaTeX" 1 -0.05))
  ("Action"
   (("g" reftex-goto-label "goto")
    ("r" reftex-query-replace-document "replace")
    ("s" counsel-rg "search")
    ("t" reftex-toc "table of content"))))

;; Hydra / Tool
(pretty-hydra-define hydra-tool
  (:hint nil :color teal :quit-key "q" :title (with-faicon "briefcase" "Tool" 1 -0.05))
  ("Network"
   (("c" ipcalc "subnet calculator")
    ("i" ipinfo "ip info"))))

;; Hydra / TypeScript
(defhydra hydra-typescript (:color blue)
  "
^
^TypeScript^          ^Do^
^──────────^──────────^──^───────────
_q_ quit             _b_ back
^^                   _e_ errors
^^                   _j_ jump
^^                   _r_ references
^^                   _R_ restart
^^                   ^^
"
  ("q" nil)
  ("b" tide-jump-back)
  ("e" tide-project-errors)
  ("j" tide-jump-to-definition)
  ("r" tide-references)
  ("R" tide-restart-server))

;; Hydra / Upload
(pretty-hydra-define hydra-upload
  (:hint nil :color teal :quit-key "q" :title (with-faicon "cloud-upload" "Upload" 1 -0.05))
  ("Action"
   (("b" webpaste-paste-buffe "buffer")
    ("i" imgbb-upload "image")
    ("r" webpaste-paste-region "region"))))

;; Hydra / Windows
(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("l" enlarge-window-horizontally "widen")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

;; General
;; =aggressive-indent-
(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))

;; =electric-operator=
(use-package electric-operator
  :delight
  :hook (python-mode . electric-operator-mode))

;; =gnuplot=
(use-package gnuplot
  :ensure-system-package gnuplot
  :defer 2)

(use-package gnuplot-mode
  :after gnuplot
  :mode "\\.gp\\'")

;; =move-text=
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

;; =paradox=
(use-package paradox
  :defer 1
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

;; =rainbow-mode=
(use-package rainbow-mode
  :delight
  :hook (prog-mode))

;; Replace the current file with the saved one
(use-package autorevert
  :ensure nil
  :delight auto-revert-mode
  :bind ("C-x R" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode 1))

;; =try=
(use-package try :defer 5)

;; =undo-tree=
(use-package undo-tree
  :delight
  :bind ("C--" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

;; =which-key=
(use-package which-key
  :defer 0.2
  :delight
  :config (which-key-mode))

;; Icons
(use-package all-the-icons :defer 0.5)

;; Ivy
(use-package counsel
  :after ivy
  :delight
  :bind (("C-x C-d" . counsel-dired-jump)
         ("C-x C-h" . counsel-minibuffer-history)
         ("C-x C-l" . counsel-find-library)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-u" . counsel-unicode-char)
         ("C-x C-v" . counsel-set-variable))
  :config (counsel-mode)
  :custom (counsel-rg-base-command "rg -S -M 150 --no-heading --line-number --color never %s"))

(use-package ivy
  :delight
  :after ivy-rich
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-i" . ivy-partial-or-done)
         ("S-SPC" . nil)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill))
  :custom
  (ivy-case-fold-search-default t)
  (ivy-count-format "(%d/%d) ")
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-pass
  :after ivy
  :commands ivy-pass)

(use-package ivy-rich
  :defer 0.1
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (replace-regexp-in-string abbreviated-home-dir "~/"
                                           (file-name-directory
                                            (directory-file-name candidate)))
                 'face 'font-lock-doc-face)
                (propertize
                 (file-name-nondirectory
                  (directory-file-name candidate))
                 'face 'success)))))

  (defun ivy-rich-compiling (candidate)
    "Displays compiling buffers of the candidate for ivy-rich."
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate)
              (not (magit-git-repo-p candidate)))
          ""
        (if (my/projectile-compilation-buffers candidate)
            "compiling"
          ""))))

  (defun ivy-rich-file-group (candidate)
    "Displays the file group of the candidate for ivy-rich"
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
               (group-function (if (fboundp #'group-name) #'group-name #'identity))
               (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))

  (defun ivy-rich-file-modes (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))

  (defun ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))

  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-rich-candidate               (:width 73))
                (ivy-rich-file-user               (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group              (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes              (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size               (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate        (:width 80))
                (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon       (:width 2))
                (ivy-rich-candidate                (:width 40))
                (ivy-rich-switch-buffer-size       (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
               :predicate (lambda (cand) (get-buffer cand))))
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

;; Linters
(use-package flycheck
  :defer 2
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-pylintrc "~/.pylintrc")
  (flycheck-python-pylint-executable "/usr/bin/pylint")
  (flycheck-stylelintrc "~/.stylelintrc.json")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; Navigation
(defun my/smarter-move-beginning-of-line (arg)
  "Moves point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'my/smarter-move-beginning-of-line)

(use-package imenu
  :ensure nil
  :bind ("C-r" . imenu))

;; Parentheses
(use-package faces
  :ensure nil
  :custom (show-paren-delay 0)
  :config
  (set-face-background 'show-paren-match "#262b36")
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff"))

;; =rainbow-delimiters=
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; =smartparens=
(use-package smartparens
  :defer 1
  :delight
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

;; Epub
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; PDF
(use-package pdf-tools
  :defer 1
  :magic ("%PDF" . pdf-view-mode)
  ;;:init (pdf-tools-install :no-query)
  )

(use-package pdf-view
  :ensure nil
  :after pdf-tools
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("d" . pdf-annot-delete)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  (pdf-view-use-unicode-ligther nil))

;; Point and Region
(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Projectile
(use-package projectile
  :defer 1
  :preface
  (defun my/projectile-compilation-buffers (&optional project)
    "Get a list of a project's compilation buffers.
If PROJECT is not specified the command acts on the current project."
    (let* ((project-root (or project (projectile-project-root)))
           (buffer-list (mapcar #'process-buffer compilation-in-progress))
           (all-buffers (cl-remove-if-not
                         (lambda (buffer)
                           (projectile-project-buffer-p buffer project-root))
                         buffer-list)))
      (if projectile-buffers-filter-function
          (funcall projectile-buffers-filter-function all-buffers)
        all-buffers)))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-keymap-prefix (kbd "C-c C-p"))
  (projectile-mode-line '(:eval (projectile-project-name)))
  :config (projectile-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

;; Recent Files
(use-package recentf
  :bind ("C-c r" . recentf-open-files)
  :init (recentf-mode)
  :custom
  (recentf-exclude (list "COMMIT_EDITMSG"
                         "~$"
                         "/scp:"
                         "/ssh:"
                         "/sudo:"
                         "/tmp/"))
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 200)
  :config (run-at-time nil (* 5 60) 'recentf-save-list))

;; Version Control
(use-package git-commit
  :after magit
  :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))

(use-package magit :defer 0.3)

(use-package smerge-mode
  :after hydra
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (hydra-merge/body)))))

(use-package git-gutter
  :defer 0.3
  :delight
  :init (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer 1
  :delight)

;; Whitespaces
(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

;; =hungry-delete=
(use-package hungry-delete
  :defer 0.7
  :delight
  :config (global-hungry-delete-mode))

;; Windows
(global-set-key [remap kill-buffer] #'kill-this-buffer)

(use-package window
  :ensure nil
  :bind (("C-x 3" . hsplit-last-buffer)
         ("C-x 2" . vsplit-last-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Gives the focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Gives the focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

;; =switch-window=
(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

;; =windmove=
(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

;; =winner=
(use-package winner
  :defer 2
  :config (winner-mode 1))

;; Word Wrap
(use-package simple
  :ensure nil
  :delight (auto-fill-function)
  :bind ("C-x p" . pop-to-mark-command)
  :hook ((prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :custom (set-mark-command-repeat-pop t))

;; YASnippet
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package yasnippet
  :delight yas-minor-mode " υ"
  :hook (yas-minor-mode . my/disable-yas-if-no-snippets)
  :config (yas-global-mode)
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1))))

(use-package ivy-yasnippet :after yasnippet)
(use-package react-snippets :after yasnippet)

;; Browser
(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "qutebrowser"))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (speed-type typit meson-mode nov dired-quick-sort org-journal org-bullets toc-org org-plus-contrib react-snippets ivy-yasnippet yasnippet-snippets switch-window hungry-delete git-timemachine git-gutter magit git-commit counsel-projectile expand-region pdf-tools smartparens rainbow-delimiters flycheck all-the-icons-ivy ivy-rich ivy-pass counsel which-key undo-tree try rainbow-mode paradox move-text gnuplot-mode gnuplot electric-operator aggressive-indent major-mode-hydra dired-subtree dired-narrow ibuffer-projectile company-box alert sql-indent web-mode ac-php company-tern tern xref-js2 js2-refactor js2-mode prettier-js scss-mode emmet-mode yaml-mode lua-mode json-mode lsp-python-ms glsl-mode cmake-ide cmake-font-lock cmake-mode clang-format+ dap-mode company-lsp lsp-ui lsp-mode solaire-mode doom-modeline doom-themes use-package-ensure-system-package delight use-package)))
 '(solair-mode-remap-fringe t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
