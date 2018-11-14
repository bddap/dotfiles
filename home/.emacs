(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'blacken)

(setq backup-directory-alist `(("." . "~/.emacs-saves/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t)))

;; (require 'lsp-mode)
;; (require 'lsp-vue)

;; check for .editorconfig
;; (add-hook 'vue-mode-hook (lambda () (editorconfig-apply)))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;; Bind C-c C-f to beautify
(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-f") #'clang-format-buffer)))
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Bind C-c C-f to py-yapf-buffer
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-f") #'blacken-buffer)))

;; (tool-bar-mode -1)
(menu-bar-mode -1)
(put 'upcase-region 'disabled nil)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(put 'downcase-region 'disabled nil)

;; apply setting from .editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-preview-eww haskell-mode toml-mode avy-flycheck dart-mode lsp-rust clang-format php-mode py-autopep8 lsp-vue editorconfig web-mode ag jedi nixos-options elpy irony mmm-mode vue-mode less-css-mode bats-mode git-blamed rust-mode markdown-mode nix-mode lsp-ui company company-irony racer auto-complete yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
