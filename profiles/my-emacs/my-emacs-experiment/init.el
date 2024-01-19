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

(set-frame-font "Iosevka 15" nil t)

(load "~/Documents/personal/my-emacs-config/elpaca-init.el")

;; install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

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
  (load "~/Documents/personal/my-emacs-config/karthink-avy-actions.el")
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

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :config
  (global-set-key (kbd "M-y") #'consult-yank-pop)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
  (global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
  (global-set-key (kbd "C-x t b") #'consult-buffer-other-tab)
  (global-set-key (kbd "M-s d") #'consult-ripgrep))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-i") #'consult-imenu)
(global-set-key (kbd "M-S-z") #'zap-up-to-char)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(put 'upcase-region 'disabled nil)

(use-package org-journal
  :init
  ;; Change default prefix key; needs to be loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/Journal/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%d.%m.%Y"))

(use-package dash)

(use-package ef-themes)

;; (require 'testing-roam-w-transient.el)
(elpaca-wait)
(load "~/Documents/personal/my-emacs-config/testing-roam-w-transient.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ec8ff5e2c8a9eb38e49a9bea6297c2194bbe0c03982630d66db1570f5ae83d90" default))
 '(custom-enabled-themes '(ef-light))
 '(display-line-numbers t)
 '(isearch-allow-motion t)
 '(package-selected-packages '(vterm))
 '(savehist-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
