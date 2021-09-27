;; -*- lexical-binding: t; -*-
(require 'skempo)

;;; https://github.com/xFA25E/nixpkgs-config/blob/master/emacs/README.org#skempo-templates

(defun skempo-user-element (arg)
  (pcase arg
    ('nix-hash (make-string 52 ?1))
    ('elisp-namespace (string-trim-right (buffer-name) (rx ".el" eos)))
    ('elisp-group (string-trim-right (buffer-name) (rx (? "-mode") ".el" eos)))
    (`(lisp-with-parens . ,body)
     (let* ((region-p (use-region-p))
            (before-p (or region-p (not (eql (char-before) ?\())))
            (after-p (or region-p (not (eql (char-after) ?\))))))
       `(l ,(when before-p "(") ,@body ,(when after-p ")"))))))

(add-to-list 'tempo-user-elements 'skempo-user-element)

;;; Lisp templates

(skempo-define-tempo (lambda :mode (emacs-lisp-mode lisp-mode))
  (lisp-with-parens
   "lambda (" p ") " n>
   r>))

(skempo-define-tempo (let :mode (emacs-lisp-mode lisp-mode))
  (lisp-with-parens
   "let ((" p "))" n>
   r>))

;;; common lisp

(skempo-define-tempo (defvar :mode lisp-mode)
  (lisp-with-parens
   "defvar " p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo (defun :mode lisp-mode)
  (lisp-with-parens
   "defun " p " (" p ")" n>
   "\"" p "\"" n>
   r>))

;;; Emacs Lisp

(skempo-define-tempo (defvar :mode emacs-lisp-mode)
  (lisp-with-parens
   "defvar " elisp-namespace "-" p n>
   r> n>
   "\"" p "\""))

(skempo-define-tempo (defun :mode emacs-lisp-mode)
  (lisp-with-parens
   "defun " elisp-namespace "-" p " (" p ")" n>
   "\"" p "\"" n>
   r>))

;;; Nix

(skempo-define-tempo (github :mode nix-mode)
  "fetchFromGitHub {" n>
  "owner = \"" p "\";" n>
  "repo = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (url :mode nix-mode)
  "fetchurl {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (zip :mode nix-mode)
  "fetchzip {" n>
  "url = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)

(skempo-define-tempo (git :mode nix-mode)
  "fetchgit {" n>
  "url = \"" p "\";" n>
  "rev = \"" p "\";" n>
  "sha256 = \"" p nix-hash "\";" n>
  "}" p >)
