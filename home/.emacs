(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'blacken)
(require 'subr-x)
(require 'json)

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
(column-number-mode 1)

(defun jump-to-file-char (path charnum)
  "open exixting file at line"
  (interactive)  
  (find-file-existing path)
  (goto-char charnum))

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process shell-file-name nil t nil shell-command-switch command))))

(defun get-next-err ()
  "determine the file-name and charater location of the next rustc error"
  (let* ((json-object-type 'hash-table)
		 (json-array-type 'list)
		 (json-key-type 'string)
		 (js (json-read-from-string (shell-command-to-string "next-rustc-err")))
		 (noth (type-of (gethash "byte_start" js)))
		 )
	(list
	 (gethash "file_name" js)
	 (gethash "byte_start" js)
	 )))

(defun next-err ()
  "go to location of next compile error"
  (interactive)
  (let ((ner (get-next-err)))
	(jump-to-file-char (nth 0 ner) (nth 1 ner))
	))

(global-set-key (kbd "\C-x n e") 'next-err)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(protobuf-mode markdown-preview-eww haskell-mode toml-mode avy-flycheck dart-mode lsp-rust clang-format php-mode py-autopep8 lsp-vue editorconfig web-mode ag jedi nixos-options elpy irony mmm-mode vue-mode less-css-mode bats-mode git-blamed rust-mode markdown-mode nix-mode lsp-ui company company-irony racer auto-complete yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
