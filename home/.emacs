(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq backup-directory-alist `(("." . "~/.emacs-saves/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t)))

;; (require 'lsp-mode)
;; (require 'lsp-vue)

;; check for .editorconfig
;; (add-hook 'vue-mode-hook (lambda () (editorconfig-apply)))

;; Bind C-c C-f to beautify
(add-hook 'c++-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-f") #'clang-format-buffer)))

;; Bind C-c C-f to py-yapf-buffer
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-f") #'py-yapf-buffer)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company yaml-mode rust-mode py-yapf nixos-options nix-mode markdown-preview-eww markdown-mode haskell-mode git-blamed bats-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
