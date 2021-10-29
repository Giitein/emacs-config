;SEC check for custom.el
(if (file-exists-p "~/.emacs.d/custom.el") nil
  (write-region "" nil "~/.emacs.d/custom.el"))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq-default frame-title-format '("%b - GNU Emacs"))

;SEC: Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;SUBSEC: list packages 
(setq package-list '(yafolding org-superstar all-the-icons use-package avy monky 
			                beacon cherry-blossom-theme kaolin-themes clues-theme company company-quickhelp dashboard
			                pdf-tools
			                evil-nerd-commenter rainbow-mode undo-tree direnv rainbow-blocks
			                solaire-mode s 
			                doom-modeline doom-themes emojify emojify-logos centaur-tabs
			                helpful highlight-indent-guides magit diff-hl minibuffer-complete-cycle free-keys 
			                paredit paredit-everywhere projectile treemacs treemacs-all-the-icons lua-mode 
			                treemacs-magit rainbow-delimiters toc-org flycheck lsp-treemacs hl-todo
			                quelpa simple-mpc function-args highlight-escape-sequences transpose-frame
			                git-commit magit-popup meson-mode helm-projectile rainbow-identifiers unicode-fonts
                                        org-drill))

;;SUBSEC: BOTSTRAPPING
(package-initialize) ;; activate all the packages (in particular autoloads)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;NOTE: general configs
;SEC: requires
(require 'doom-modeline)
(require 'dashboard)
(straight-use-package 'helm)
(require 'helm)
(require 'helm-config)
(require 'transpose-frame)
(require 'projectile)
(require 'centaur-tabs)
(require 'rainbow-mode)
(require 'rainbow-blocks)
(require 'function-args)



;SEC: util
(global-display-line-numbers-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook #'global-emojify-mode)
(show-paren-mode 1)
(global-undo-tree-mode)
(normal-erase-is-backspace-mode 1)
;; in line function arguments hint
(fa-config-default)
;;SUBSEC: movement
(global-set-key (kbd "C-x p") 'move-to-window-line-top-bottom)
(global-set-key (kbd "C-:") 'avy-goto-char)
;;SUBSEC: backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;;SUBSEC:nerd commenter
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
;;SUBSEC: pastebin
(use-package webpaste
  :ensure t
  :bind (("C-c C-p" . webpaste-paste-buffer)
         ("C-c C-r" . webpaste-paste-region))
  :config
  (progn
    (setq webpaste-provider-priority '("ix.io" "dpaste.org" "dpaste.com"))))



;SEC: Visual stuff
(global-hl-line-mode +1)
(setq column-number-mode t)
(solaire-global-mode +1)
(beacon-mode 1)
;(tool-bar-mode -1)
(doom-modeline-mode 1)
(global-diff-hl-mode)
(diff-hl-margin-mode)
(put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
(put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)
(set-frame-parameter (selected-frame) 'alpha '(98 . 90))
(add-to-list 'default-frame-alist '(alpha . (98 . 90)))
;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)
;;SUBSEC: centaur tabs
(centaur-tabs-mode t)
(global-set-key (kbd "M-,")  'centaur-tabs-backward)
(global-set-key (kbd "M-.") 'centaur-tabs-forward)
;;SUBSEC:colors
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
;;SUB SUBSEC: highlight TODO
(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FEF000")
        ("NOTE"   . "#BCAFFF")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#ff99d3")
        ("STUB"   . "#1E90FF")
        ("SEC"    . "#F8FF2b")
        ("SUBSEC" . "#BBFF00")
        ("XXX"    . "#FF4500")))
(setq org-ellipsis " ⤵")


;;SEC:DASHBOARD
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/sml-logo.png")
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer
(setq dashboard-center-content t)
(setq dashboard-week-agenda t)



;SEC: Parens
(electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(setq electric-pair-delete-adjacent-pair nil)



;SEC: indentation
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)
;SUBSEC: yafolding
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)



;SEC: projectile
(projectile-global-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
;;TODO: fix these paths
(setq projectile-project-search-path '("~/Documents/programming/C/pj" "~/Documents/programming/cxx/xxpj/"))



;SEC: MAGIT
(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)

  [remap shell-command] 'with-editor-shell-command)



;;SEC: HELM
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c b") 'helm-occur)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-M-x-fuzzy-match t)
(helm-mode 1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(load-theme 'doom-gruvbox t)
;;SEC: FILE LOADING
(load "~/.emacs.d/org.el")


;;SEC: Keybindings
(global-set-key (kbd "C-c M-c") 'compile)
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(autoload 'View-scroll-page-forward "view")
(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;2B" [S-down])
(autoload 'View-scroll-page-backward "view")
(global-set-key (kbd "C-d") 'View-scroll-half-page-forward)
(global-set-key (kbd "C-u") 'View-scroll-half-page-backward)


;;SEC: etcetera
(require 'treemacs-all-the-icons)
(treemacs-load-theme "all-the-icons")

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;; SEC: Font
(set-face-attribute 'default nil :family "CaskaydiaCove NF" :height 120)

;;SEC: ORG
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
