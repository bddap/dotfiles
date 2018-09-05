(setq backup-directory-alist `(("." . "~/.emacs-saves/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t)))

(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; https://medium.com/@aria_39488/improving-vue-mode-for-better-vue-js-editing-inside-of-spacemacs-4509f0577ea0
;; (require 'vue-mode)
;; (require 'vue-mode)
;;   (add-to-list 'vue-mode-hook #'smartparens-mode)
(require 'lsp-mode)
(require 'lsp-vue)
;; (add-hook 'vue-mode-hook #'lsp-vue-mmm-enable)
;; (require 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-flycheck))
;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

;; check for .editorconfig
(add-hook 'vue-mode-hook (lambda () (editorconfig-apply)))

;; Bind C-c C-f to beautify
(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-f") #'clang-format-buffer)))

;; Bind C-c C-f to py-yapf-buffer
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-f") #'py-yapf-buffer)))

;; Bind C-c C-f to py-yapf-buffer
;; doesn't work
;; (add-hook 'elpy-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "\C-c\C-f") #'py-yapf-buffer)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-project-whitelist (quote ("^/Users/a/d/jack/jack-web/$")))
 '(package-selected-packages
   (quote
    (yaml-mode toml-mode elpy jedi company auto-complete racer dart-mode py-yapf less-css-mode lsp-rust web-mode py-autopep8 editorconfig lsp-ui avy-flycheck vue-mode mmm-mode lsp-vue php-mode clang-format ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
