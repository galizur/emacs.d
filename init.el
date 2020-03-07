;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

;; Package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; Use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package delight :ensure t)
(use-package use-package-ensure-system-package :ensure t)

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

;; Fonts
(set-face-attribute 'default nil :font "Source Code Pro Medium")
(set-fontset-font t 'latin "Noto Sans")

;; Theme
(use-package doom-themes
  :config (load-theme 'doom-one t))
;; All the icons
(use-package all-the-icons)
;; Modeline
(use-package doom-modeline
  :config (doom-modeline-mode))
;; Solaire mode (makes certain parts brighter)
(use-package solaire-mode
  :config
  (solaire-mode-swap-bg)
  (solaire-global-mode +1))

;; Turn off mouse interface
(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; Keyboard shortcuts
(use-package which-key
  :delight
  :config (which-key-mode))

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

;; File explorer
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

;; Syntax checking
(use-package flycheck
  :delight
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))

;; Generic completion
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
         ("M-H" . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-i" . ivy-partial-or-done)
         ("S-SPC" . nil)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill))
  :custom
  (ivy-case-fold-search-default t)
  (ivy-count-foarmt "(%d/%d) ")
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-pass
  :after ivy
  :commands ivy-pass)

(use-package ivy-rich
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (preplace-regexp-in-string abbreviated-home-dir "~/"
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

  (defun ivy-rich-file-mode (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))

  (defun ivy-rich-file-size (candidate)
    "Displayes the file size of the candidate for ivy-rich."
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
    "Returns an icon for the candidate out of 'all-the-icons'."
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
               ((ivy-rich-candidate (:width 73))
                (ivy-rich-file-user (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate (:width 80))
                (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon (:width 2))
                (ivy-rich-candidate (:width 40))
                (ivy-rich-switch-buffer-size (:width 7))
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

;; Auto-completion
(use-package company
  :delight
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))
(use-package company-box
  :after company
  :delight
  :hook (company-mode . company-box-mode))

;; Alert
(use-package alert
  :custom (alert-default-style 'libnotify))

;; History
(use-package savehist
  :ensure nil
  :custom
  (history-delete-duplicates t)
  (history-length t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-save-minibuffer-history 1)
  :config (savehist-mode 1))

;; Navigation
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move pint to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace and
the beginning of the line.

If ARG is not nil or 1, move forward ARG -1 lines first.  If
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

;; Move text
(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :config (move-text-default-bindings))

;; Point and region
(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning)(region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Improved package menu
(use-package paradox
  :custom
  (paradox-column-width-package 27)
  (paradox-column-width-version 13)
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

;; Replace current file with saved one
(use-package auto-revert
  :ensure nil
  :delight auto-revert-mode
  :bind ("C-x R" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode 1))

;; Undo
(use-package undo-tree
  :delight
  :bind ("C--" . undo-tree-redo)
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

;; Windows
;; Don't ask before killing a buffer.
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

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-x w" . switch-window-then-swap-buffer)))

(use-package windmove
  :bind (("C-c h" . windmove-left)
         ("C-c j" . windmove-down)
         ("C-c k" . windmove-up)
         ("C-c l" . windmove-right)))

(use-package winner
  :config (winner-mode 1))

;; Word-wrap
(use-package simple
  :ensure nil
  :delight (auto-fill-function)
  :bind ("C-x p" . pop-to-mark-command)
  :hook ((prog-mode . turn-on-auto-fill)
         (text-mode . turn-on-auto-fill))
  :custom (set-mark-command-repeat-pop t))

;; Dashboard
(use-package dashboard
  :if (< (length command-line-args) 2)
  :preface
  (defun dashboard-load-packages (list-size)
    (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 5) ? )
            (format "%d packages loaded in %s" (length package-activated-list) (emacs-init-time))))
  :custom
  (dashboard-banner-logo-title "Coding Is What We Do")
  (dashboard-center-content t)
  (dashboard-items '((pachages)
                     (agenda)
                     (projects .5)))
  (dashboard-navigator-buttons
   `(
     (,(and (display-graphic-p)
            (all-the-icons-faicon "gitlab" :height 1.2 :v-adjust -0.1))
      "Homepage"
      "Browse Homepage"
      (lambda (&rest _) (browse-url homepage)))
     (,(and (display-graphic-p)
            (all-the-icons-material "update" :height 1.2 :v-adjust -0.24))
      "Update"
      "Update emacs"
      (lambda (&rest _) (auto-package-update-now)))))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info nil)
  (dashboard-set-navigator t)
  (dashboard-startup-banner 'logo)
  :config
  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))
  (dashboard-setup-startup-hook))

;; Hydra
(use-package hydra
  :bind (("C-c M" . hydra-merge/body)
         ("C-c T" . hydra-tool/body)
         ("C-c b" . hydra-btoggle/body)
         ("C-c c" . hydra-clock/body)
         ("C-c f" . hydra-flycheck/body)
         ("C-c g" . hydra-go-to-file/body)
         ("C-c m" . hydra-magit/body)
         ("C-c o" . hydra-org/body)
         ("C-c p" . hydra-projectile/body)
         ("C-c w" . hydra-windows/body)))

(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icons."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " "
              str))
  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " "
              str))
  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the Github Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " "
              str)))

;; Hydra/BToggle
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

;; Hydra/Clock
(pretty-hydra-define hydra-clock
  (:hint nil :color teal :quit-key "q" :title (with-faicon "clock-o" "Clock" 1 -0.05))
  ("Action"
   (("c" org-clock-cancel "cancel")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("r" org-clock-report "report"))))

;; Hydra/Flycheck
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

;; Hydra/Magit
(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

;; Hydra/Merge
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

;; Hydra/Org
(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (("A" my/org-archive-done-tasks "archive")
    ("a" org-agenda "agenda")
    ("c" org-capture "capture")
    ("d" org-decrypt-entry "decrtypt")
    ("i" org-insert-link-global "insert-link")
    ("j" my/org-jump "jump-task")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("s" org-store-link "store-link")
    ("t" org-show-todo-tree "todo-tree"))))

;; Hydra/Projectile
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
    ("R" projectile-repace-regexp "regexp replace")
    ("s" counsel-rg "search"))))

;; Hydra/Windows
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

;; PDF
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-tools-install :no-query))

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
  (pdf-view-use-unicode-lighter nil))

;; Async
(use-package async)

;;; Languages
;; C++
(use-package ccls
  :after projectile
  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
(setq clang-format-style-option "file")

;; E-lisp
(use-package elisp-mode :ensure nil :delight "ξ ")
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
    "Prints the arrays of numbers in one line."
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

;; Markdown
(use-package markdown-mode
  :ensure-system-package (pandoc . "trizen -S pandoc")
  :delight "μ "
  :mode ("\\.markdown\\'" "\\.md\\'")
  :custom (markdown-command "/usr/bin/pandoc"))

(use-package markdown-preview-mode
  :after markdown-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
$(document).on('mdContentChange', function () {
$('pre code').each(function(i, block) {
hljs.highlightBlock(block);
});
});
</script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")
         "<style>
.markdown-body {
box-sizing: border-box;
min-width: 200px;
max-width: 980px;
margin: 0 auto;
padding: 45px;
}
@media (max-width: 767px) { .markdown-body {padding: 15px; }}
</style>")))

;; Shell-script
(use-package sh-script
  :ensure nil
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;;; General coding
;; LSP (Language server protocol)
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

;; Auto indent
(use-package aggressive-indent
  :hook ((css-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (js-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode))
  :custom (aggressive-indent-comments-too))
(use-package electric-operator
  :delight
  :hook (python-mode . electric-operator-mode))

;; Colorize colors
(use-package rainbow-mode
  :delight
  :hook (prog-mode))

;; Parentheses
(use-package faces
  :ensure nil
  :custom (show-paren-delay 0)
  :config
  (set-face-background 'show-paren-match "#262b36")
  (set-face-bold 'show-paren-match t)
  (set-face-foreground 'show-paren-match "#ffffff"))
(use-package smartparens
  :delight
  :custom (sp-escape-quotes-after-insert nil)
  :config (smartparens-global-mode 1))

;; Projects
(use-package projectile
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

;; Version control
(use-package git-commit
  :after magit
  :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun my/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 72 characters."
    (setq fill-column 72)
    (setq-local comment-auto-fill-only-comments nil)))

(use-package magit)

(use-package smerge-mode
  :after hydra
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (hydra-merge/body)))))
(use-package git-gutter
  :delight
  :init (global-git-gutter-mode +1))
(use-package git-timemachine
  :delight)

;; Whitespaces
(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace))

(use-package hungry-delete
  :delight
  :config (global-hungry-delete-mode))

;; Snippets
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package yasnippet
  :delight yas-minor-mode " u"
  :hook (yas-minor0mode . my/disable-yas-if-no-snippets)
  :config (yas-global-mode)
  :preface
  (defun my/disable-yas-if-no-snippets ()
    (when (and yas-minor-mode (null (yas--get-snippet-tables)))
      (yas-minor-mode -1))))

(use-package ivy-yasnippet :after yasnippet)
(use-package react-snippets :after yasnippet)

;; Calculator
(use-package calc
  :defer t
  :custom
  (math-additional-units
   '((GiB "1024 * MiB" "Giga Byte")
     (MiB "1024 * KiB" "Mega Byte")
     (KiB "1024 * B" "Kilo Byte")
     (B nil "Byte")
     (Gib "1024 * Mib" "Giga Bit")
     (Mib "1024 * Kib" "Mega Bit")
     (Kib "1024 * b" "Kilo Bit")
     (b "B / 8" "Bit")))
  (math-units-table nil))

;;; Org-Mode
(use-package org
  :ensure org-plus-contrib
  :delight "Θ "
  :bind ("C-c i" . org-insert-structure-template)
  :preface
  (defun my/org-compare-times (clocked estimated)
    "Gets the ratio between the timed time and the estimated time."
    (if (and (> (length clocked) 0) estimated)
        (format "%.2f"
                (/ (* 1.0 (org-duration-to-minutes clocked))
                   (org-duration-to-minutes estimated)))
      ""))
  (defun my/org-archive-done-tasks ()
    "Archives finished or cancelled tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "TODO=\"DONE\"|TODO=\"Cancelled\"" (if (org-before-first-heading-p) 'file 'tree)))
  (defun my/org-jump ()
    "Jumps to a specific task."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-refile)))
  (defun my/org-use-speed-commands-for-headings-and-lists ()
    "Activates speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**"))
        (save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*")))))
  (defmacro ignore-args (fnc)
    "Returns function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))
  :hook ((after-save . my/config-tangle)
         (auto-save . org-save-all-org-buffers)
         (org-mode . org-indent-mode))
  :custom
  (org-blank-before-new-entry '((headint . t)
                                (plain-list-item . t)))
  (org-cycle-include-plain-lists 'integrate)
  (org-expiry-inactive-timestamps t)
  (org-export-backends '(ascii beamer html icalendar latex man md org texinfo))
  (org-log-don 'time)
  (org-log-into-drawer "LOGBOOK:")
  (org-modules '(org-crypt
                 org-habit
                 org-info
                 org-irc
                 org-mouse
                 org-protocol
                 org-tempo))
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-cache nil)
  (org-refile-use-outline-path nil)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-startup-folder nil)
  (org-startup-with-inline-images t)
  (org-tag-alist '(("@coding" . ?c)
                   ("@computer" . ?l)
                   ("@errands" . ?e)
                   ("@home" . ?h)
                   ("@phone" . ?p)
                   ("@reading" . ?r)
                   ("@school" . ?s)
                   ("@work" . ?b)
                   ("@writing" . ?w)
                   ("crypt" . ?C)
                   ("fuzzy" . ?0)
                   ("highenergy" . ?1)))
  (org-tags-exclude-from-inheritance '("crypt" "project"))
  (org-todo-keywords '((sequence "TODO(t)"
                                 "STARTED(s)"
                                 "WAITING(w@/!)"
                                 "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
                       (sequence "TOBUY"
                                 "TOSHRINK"
                                 "TOCUT"
                                 "TOSEW" "|" "DONE(x)")))
  (org-use-effective-time t)
  (org-use-speed-commands 'my/org-use-speed-commands-for-headings-and-lists)
  (org-yank-adjusted-subtrees t)
  :config
  (add-to-list 'org-global-properties '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))
  (add-to-list 'org-speed-commands-user '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands-user '("$" call-interactively 'org-archive-subtree))
  (add-to-list 'org-speed-commands-user '("d" my/org-move-line-to-destination))
  (add-to-list 'org-speed-commands-user '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands-user '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
  (advice-add 'org-deadline :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-schedule :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (ignore-args #'org-save-all-org-buffers))
  (advice-add 'org-todo :after (ignore-args #'org-save-all-org-buffers))
  (org-clock-persistence-insinuate)
  (org-load-modules-maybe t))

;; Auto table of contents
(use-package toc-org
  :after org
  :hook (org-mode . toc-org-enable))

;; Cleaner online mode
(use-package org-indent :ensure nil :after org :delight)

;; Prettier bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "►" "▸")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pdf-tools clang-format clang-format+ org-bullets toc-org org-plus-contrib major-mode-hydra dashboard markdown-preview-mode ccls dap-mode company-lsp lsp-ui lsp-mode react-snippets ivy-yasnippet yasnippet-snippets yasnippet switch-window hungry-delete git-timemachine git-gutter magit counsel-projectile expand-region smartparens smarparens all-the-icons-ivy ivy-rich ivy-pass counsel undo-tree rainbow-mode paradox move-text electric-operator aggressive-indent async ibuffer-projectile company-box company alert lua-mode json-mode dired-subtree dired-narrow which-key flycheck solaire-mode doom-modeline all-the-icons doom-themes doom-one use-package-ensure-system-package delight use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
