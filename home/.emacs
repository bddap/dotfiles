(package-initialize)

(custom-set-variables
 ;; not meant for hand editing
 '(package-selected-packages
   (quote
    (go-mode company-lsp yasnippet spinner lsp-mode dockerfile-mode protobuf-mode markdown-preview-eww haskell-mode toml-mode avy-flycheck dart-mode lsp-rust clang-format php-mode py-autopep8 lsp-vue editorconfig web-mode ag jedi nixos-options elpy irony mmm-mode vue-mode less-css-mode bats-mode git-blamed rust-mode markdown-mode nix-mode lsp-ui company company-irony racer auto-complete yaml-mode))))
(custom-set-faces
 ;; not meant for hand editing
 )

(setq package-check-signature nil)
(setq package-archives
      '(("melpa"        . "~/.emacs.d/elpa-mirror/melpa/")
		("stable-melpa" . "~/.emacs.d/elpa-mirror/stable-melpa/")
        ("org"          . "~/.emacs.d/elpa-mirror/org/")
        ("gnu"          . "~/.emacs.d/elpa-mirror/gnu/")))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-refresh-contents) ;; fast because local storage
(package-install-selected-packages) ;; fast because local storage
(setq backup-directory-alist `(("." . "~/.emacs.d/saves/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/" t)))

(require 'blacken)
(require 'subr-x)
(require 'json)
(require 'company-lsp)
(require 'yasnippet)
(require 'lsp)
(require 'lsp-clients)
(require 'rust-mode)
(require 'editorconfig)
;; (require 'lsp-mode)
;; (require 'lsp-vue)

(push 'company-lsp company-backends)
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;; Bind C-c C-f to beautify
(add-hook 'c++-mode-hook (lambda () (local-set-key (kbd "\C-c\C-f") #'clang-format-buffer)))
(add-hook 'c-mode-hook (lambda () (local-set-key (kbd "\C-c\C-f") #'clang-format-buffer)))
(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "\C-c\C-f") #'blacken-buffer)))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'python-mode-hook 'lsp) 
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq company-tooltip-align-annotations t)
(editorconfig-mode 1)
(column-number-mode 1)
(menu-bar-mode -1)

(defun jump-to-file-char (path charnum)
  "open existing file at line"
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
    (jump-to-file-char (nth 0 ner) (+ 1 (nth 1 ner)))
    ))

;; todo: add-hook, local-set-key
(global-set-key (kbd "\C-x n e") 'next-err)
