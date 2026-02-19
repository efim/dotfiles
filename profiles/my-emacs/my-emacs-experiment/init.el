;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 John Doe
;;
;; Author: John Doe <john@doe.com>
;; Maintainer: John Doe <john@doe.com>
;; Created: January 03, 2024
;; Modified: January 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/enefedov/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(set-frame-font "Iosevka Comfy 14" nil t)

(load "~/.config/my-emacs-experiment/elpaca-init.el")

;; install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
;; issue with workaround https://github.com/progfolio/elpaca/issues/216
;; issue to track about updating built-ins https://github.com/progfolio/elpaca/issues/236tt
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq :elpaca `(seq :build ,(+elpaca-seq-build-steps)))

(use-package transient)

(use-package magit
  :after transient)



(use-package emacs :elpaca nil
  :config
  (global-unset-key (kbd "C-<backspace>"))
  (tab-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tab-bar-history-mode 1)
  (setq view-read-only 1) ; automatically open read-only files in `view' mode
  (setq next-screen-context-lines 10)
  (setq isearch-allow-scroll t) ; movements inside isearch
  (setq isearch-allow-motion t) ; movements inside isearch  
  (add-to-list 'tab-bar-format #'tab-bar-format-menu-bar)
  :bind (("M-d" . kill-region)
			("C-w" . backward-kill-word)
			("M-o" . other-window)
			("M-i" . consult-imenu)
			("M-z". zap-to-char)				 ; more common that i intend to use to kill arguments (M-b M-z ,)
			("M-C-z". zap-up-to-char)
			("C-x C-z" . suspend-frame)
			("C-z" . repeat)
			("C-S-z" . repeat-complex-command)))

(use-package treesit
  :ensure nil
  :preface
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq recommended-tree-sitter-sources '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (scala "https://github.com/tree-sitter/tree-sitter-scala")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    ; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq treesit-language-source-alist recommended-tree-sitter-sources)
  (dolist (mapping
           '((scala-mode . scala-ts-mode)
             (go-mode . go-ts-mode)
             (json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package combobulate
  :ensure (combobulate :host github :repo "mickeynp/combobulate")
  :after treesit
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode)))

(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-'") 'avy-goto-char)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-\"") 'avy-goto-char-timer)
  
  (global-set-key (kbd "M-j") 'avy-goto-char-timer)
  (define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

  (global-set-key (kbd "M-g M-g") 'avy-goto-line)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (load "~/.config/my-emacs-experiment/karthink-avy-actions.el")
  )

(use-package vertico
  :init
  (vertico-mode)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  (define-key global-map (kbd "C-c r") #'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (keymap-set vertico-map "M-P" #'vertico-repeat-previous)
  (keymap-set vertico-map "M-N" #'vertico-repeat-next)
  )

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; the ui for completion at point (C-M-i or M-TAB)
; for orderless separated default is space, inserted by M-SPC
(use-package corfu
  :init
  (global-corfu-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)	      ;; pick some comfortable binding
	 ("C-;" . embark-dwim)	      ;; good alternative: M-.
	 ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  :config
  (global-set-key (kbd "M-y") #'consult-yank-pop)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
  (global-set-key (kbd "C-x t b") #'consult-buffer-other-tab)
  (global-set-key (kbd "M-s d") #'consult-ripgrep)
  :bind (("C-x j" . #'consult-mark)
	 ("C-x J" . #'consult-global-mark)))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)
	 ("C-h x" . #'helpful-command)
	 :map help-map
	      ("o" . helpful-symbol)
	 :map emacs-lisp-mode-map
	      ("C-c C-d" . helpful-at-point)))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(put 'upcase-region 'disabled nil)

(use-package dired :elpaca nil
      :init
      (add-hook 'dired-mode-hook #'hl-line-mode)
		:config
		(setq dired-dwim-target t) 		 ; auto insert target dir when Copy with two dired buffers opened
		)

(use-package org
  :elpaca nil
  :config
  (setq org-use-speed-commands 't)
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (shell . t))))

(use-package org-journal
  :init
  ;; Change default prefix key; needs to be loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/Journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%d.%m.%Y"))

(use-package dash)

(use-package ef-themes
  :config
  (setq ef-themes-to-toggle '(ef-light ef-night)))

;;; here will be stuff about coding

(use-package repeat
  :elpaca nil
  :config
  (repeat-mode 1))

(use-package git-gutter
  :config
  (global-git-gutter-mode t)
  :bind (("M-g h n" . 'git-gutter:next-hunk)
			("M-g h p" . 'git-gutter:previous-hunk)
			("M-g h r" . 'git-gutter:revert-hunk)
			("M-g h SPC" . 'git-gutter:mark-hunk)
			:repeat-map my/git-gutter-repeat-map
			("n" . 'git-gutter:next-hunk)
			("p" . 'git-gutter:previous-hunk)
			("r" . 'git-gutter:revert-hunk)))

(use-package magit
  :config
  (setq magit-define-global-key-bindings 'recommended))
  ;; sets C-x g for magit status, C-c g for dispatch, and C-c f for file dispatch)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package go-mode
  :hook
  (go-mode . eglot-ensure)
  :bind
  (:map go-mode-map ("C-c C-d" . #'eldoc-doc-buffer)))

(use-package sly
  :config
  (setq inferior-lisp-program "/usr/bin/env sbcl"))

(use-package templ-ts-mode)

(defun setup-scala-buffer ()
  (indent-tabs-mode -1)
  (setq next-error-function #'flymake-goto-next-error)
  (message "scala buffer is setup"))

(use-package scala-ts-mode
  :init
  (push `(scala-ts-mode . ,(alist-get 'scala-mode eglot-server-programs))
		  eglot-server-programs)
  :hook
  (scala-ts-mode . eglot-ensure)
  (scala-ts-mode . setup-scala-buffer))

(use-package ensime-mode
  :ensure nil
  :elpaca nil
  :load-path "~/Documents/repos-other/ensime-tng-3.0.15/lisp/"
  ;; :load-path "~/Downloads/ensime-tng-3.0.15/lisp/"
  :commands ensime-mode
  :bind
  (:map ensime-mode-map
        ("M-." . ensime-jump-to-definition)
        ("C-c C-i t" . ensime-type-at-point)
        ("C-c C-i s" . ensime-symbol-at-point)
        ("C-c C-r i" . ensime-import-symbol-at-point)))

(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)
(use-package consult-yasnippet)

(use-package envrc
  :config
  (envrc-global-mode))

(use-package cape)
(use-package company-web)		; this pulls in company in full
;; https://github.com/smihica/emmet-mode#html-abbreviations
(use-package emmet-mode
  :hook
  (web-mode . emmet-mode))

;; https://kristofferbalintona.me/posts/202203130102/
(defun my/cape-capf-setup-web-mode ()
  (let ((result))
    (dolist (element (list
		      #'cape-dabbrev
		      (cape-company-to-capf #'company-web-html)
		      (cape-company-to-capf #'company-css))
		      result)
      (add-to-list 'completion-at-point-functions element))))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.gohtml?\\'" . web-mode))
  :hook (web-mode . my/cape-capf-setup-web-mode))

(defun my/setup-eat-visual ()
  "Configure minor modes for hooking into eat-mode."
  (display-line-numbers-mode 0))
(defun my/new-eat-other-frame ()
  "Call the `eat` with single universal argument in a new frame."
  (interactive)
  (select-frame (make-frame))
  (let ((current-prefix-arg '(4)))
    (call-interactively #'eat)
	 (toggle-frame-tab-bar)
	  (set-frame-parameter (selected-frame) 'alpha 90)))
(defun my/eat-kill-frame ()
  "Send stop, kill buffer and frame."
  (interactive)
  (call-interactively #'eat-kill-process)
  (kill-buffer (current-buffer))
  (call-interactively #'delete-frame))

(use-package eat
  :bind
  (("C-c j" . avy-goto-char-timer)
   ("C-c T" . my/new-eat-other-frame)
   ("C-c C-q" . my/eat-kill-frame))
  (:repeat-map my/eat-repeat-map
	       ("p" . eat-previous-shell-prompt)
	       ("n" . eat-next-shell-prompt))
  :hook
  ((eat-mode . my/setup-eat-visual)))

(electric-pair-mode)
;;; end of coding things

;; creation of html files from buffers, with themed styling
(use-package htmlize)

(use-package writeroom-mode)

;; https://www.masteringemacs.org/article/re-builder-interactive-regexp-builder
(use-package re-builder :elpaca nil
  :config
  (setq reb-re-syntax 'string))

(use-package flyspell
  :elpaca nil
  :hook (org-mode org-journal-mode))

(use-package eww
  :elpaca nil
  :config
  (load "~/.config/my-emacs-experiment/eww-mozilla-readability.el"))

;;
;;; --- Tab-Based View Mode Persistence ---

(defun my/get-current-tab-data ()
  "Get the actual (mutable) alist for the current tab."
  (let* ((tabs (tab-bar-tabs))
         (index (tab-bar--current-tab-index tabs)))
    (nth index tabs)))

(defun my/sync-tab-view-mode ()
  "Match current buffer's `view-mode` to the current tab's 'tab-view-mode' parameter."
  (when (and buffer-file-name (not (minibufferp)))
    (let* ((current-tab (my/get-current-tab-data))
           (tab-requires-view (alist-get 'tab-view-mode (cdr current-tab))))
      (if tab-requires-view
          (unless view-mode (view-mode 1))
        ;; Only turn off view-mode if it was likely this system that enabled it
        (when (and view-mode (bound-and-true-p view-mode))
          (view-mode -1))))))

(defun my/toggle-tab-view-mode ()
  "Toggle the 'view' state for the current tab and update visibility."
  (interactive)
  (let* ((tabs (tab-bar-tabs))
         (index (tab-bar--current-tab-index tabs))
         (tab (nth index tabs))
         (new-state (not (alist-get 'tab-view-mode (cdr tab)))))
    ;; Correctly update the master tab list
    (setf (alist-get 'tab-view-mode (cdr tab)) new-state)
    ;; Apply to current buffer
    (my/sync-tab-view-mode)
    ;; Refresh UI
    (force-mode-line-update t)
    (message "Tab View State: %s" (if new-state "READ-ONLY (Review)" "EDITABLE"))))

;;; --- UI Enhancements ---

(setq tab-bar-tab-name-format-function
      (lambda (tab i)
        (let ((name (tab-bar-tab-name-format-default tab i)))
          (if (alist-get 'tab-view-mode (cdr tab))
               (concat "📖 " name)
            name))))

;;; --- Activation ---

;; Catch window/buffer changes
(add-hook 'window-configuration-change-hook #'my/sync-tab-view-mode)

;; Explicitly catch tab switching (important!)
(add-hook 'tab-bar-tab-post-select-functions (lambda (&rest _) (my/sync-tab-view-mode)))

;; Keybinding
(define-key global-map (kbd "C-x t V") #'my/toggle-tab-view-mode)

(elpaca-wait)
(load "~/.config/my-emacs-experiment/testing-roam-w-transient.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(custom-safe-themes '("cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37" "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53" default))
 '(custom-enabled-themes '(ef-light))
 '(display-line-numbers t)
 '(isearch-allow-motion t)
 '(package-selected-packages '(vterm))
 '(savehist-mode t)
 '(tab-width 3))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
