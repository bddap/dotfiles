(package-initialize)

(setq package-check-signature nil)
(setq package-archives '(("melpa"        . "~/.emacs.d/elpa-mirror/melpa/")
						 ;; ("stable-melpa" . "~/.emacs.d/elpa-mirror/stable-melpa/")
						 ;; ("org"          . "~/.emacs.d/elpa-mirror/org/")
						 ;; ("gnu"          . "~/.emacs.d/elpa-mirror/gnu/")
						 ))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq backup-directory-alist `(("." . "~/.emacs.d/saves/bak/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/auto/" t)))
(setq create-lockfiles nil) ; stop emacs from creating '.#filename' files becuase they mess with
										; filesystem watchers

(require 'lsp)
(require 'lsp-mode)
(require 'editorconfig)
(require 'use-package)
(require 'yasnippet)
(yas-global-mode 1)

;; use the language server protocol whenever possible
;; (add-hook 'prog-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'sh-mode-hook #'lsp)
(add-hook 'dockerfile-mode-hook #'lsp)
(add-hook 'c-mode-common-hook #'lsp)
(add-hook 'lsp-ui-mode-hook 'lsp-ui-doc-hide)

(define-key prog-mode-map (kbd "C-c f") #'project-find-file)

(define-key lsp-mode-map (kbd "C-c u") #'lsp-rename)
(define-key lsp-mode-map (kbd "C-c i") #'lsp-ui-peek-find-references)
(define-key lsp-mode-map (kbd "C-c o") #'lsp-ui-doc-glance)
(define-key lsp-mode-map (kbd "C-c l") #'company-complete-common)
(define-key lsp-mode-map (kbd "C-c y") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "M-n") #'next-error)
(define-key lsp-mode-map (kbd "M-p") #'previous-error)
(define-key lsp-mode-map (kbd "C-c h") #'flycheck-first-error)
(define-key lsp-mode-map (kbd "C-c C-f") #'lsp-format-buffer)

;; pyright can't format so we define a custom keybind in python mode
(add-hook 'python-mode-hook (lambda () 
							  (progn (define-key lsp-mode-map (kbd "C-c C-f") nil) 
									 (local-set-key (kbd "C-c C-f") #'py-yapf-buffer))))

(add-hook 'emacs-lisp-mode-hook (lambda () 
								  (local-set-key (kbd "C-c C-f") #'elisp-format-buffer)))

;; stop typescript lsp from adding '.log' file to pwd
;; https://github.com/emacs-lsp/lsp-mode/issues/1490
(use-package 
  lsp-mode 
  :hook (web-mode . lsp) 
  :custom (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")))
;; js-mode binds "M-." to js-find-symbol. We don't want that because lsp-goto-implementation is better.
(add-hook 'js-mode-hook (lambda () 
						  (local-set-key (kbd "M-.") 'lsp-goto-implementation)))

;; keep lsp-ui-doc from popping up without my say-so
(setq lsp-ui-doc-enable nil)
(setq lsp-file-watch-threshold 100000)
(use-package 
  lsp-ui 
  :ensure t 
  :after lsp-mode 
  :init (setq lsp-ui-doc-enable nil) 
  (setq lsp-signature-render-documentation nil))

(use-package 
  lsp-pyright 
  :ensure t 
  :hook (python-mode . (lambda () 
						 (require 'lsp-pyright) 
						 (lsp))))		; or lsp-deferred

(add-hook 'markdown-mode-hook (lambda () 
								(visual-line-mode)))

;; Todo, unbind TAB from company-complete-common, use C-c l instead

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\justfile\\'" . makefile-mode))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq company-tooltip-align-annotations t)
(editorconfig-mode 1)
(column-number-mode 1)
(menu-bar-mode -1)
(ivy-mode)
;; https://github.com/rust-lang/rust-mode#indentation
(add-hook 'rust-mode-hook (lambda () 
							(setq indent-tabs-mode nil)))
(setq lsp-prefer-flymake nil) ;; tell lsp to use flycheck instead
;; https://users.rust-lang.org/t/how-to-disable-rust-analyzer-proc-macro-warnings-in-neovim/53150/8
(setq lsp-rust-analyzer-proc-macro-enable t)
(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
;; (setq lsp-rust-analyzer-server-display-inlay-hints t)

(require 'json)
(defun jump-to-file-char (path charnum) 
  "open existing file at line" 
  (interactive) 
  (find-file-existing path) 
  (goto-char charnum))

(defun shell-command-to-string (command) 
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string (with-current-buffer standard-output (call-process shell-file-name nil t
																			nil shell-command-switch
																			command))))

;; (defun bddap-lsp-config ()
;;   "set lsp config"
;;   (interactive)
;;   (lsp--set-configuration `(:rust-analyzer (:cargo (:allFeatures (true))))))

(defun get-next-err () 
  "determine the file-name and charater location of the next rustc error"
  (let* ((json-object-type 'hash-table) 
		 (json-array-type 'list) 
		 (json-key-type 'string) 
		 (js (json-read-from-string (shell-command-to-string "next-rustc-err"))) 
		 (noth (type-of (gethash "byte_start" js)))) 
	(list (gethash "file_name" js) 
		  (gethash "byte_start" js))))

(defun next-err () 
  "go to location of next compile error" 
  (interactive) 
  (let ((ner (get-next-err))) 
	(jump-to-file-char (nth 0 ner) 
					   (+ 1 (nth 1 ner)))))

;; todo: add-hook, local-set-key
(global-set-key (kbd "\C-x n e") 'next-err)
(global-set-key (kbd "\C-c C-s") 'swiper-isearch)
(global-set-key (kbd "\C-c C-r") 'swiper-isearch-reverse)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-enable nil) 
 '(package-selected-packages (quote (elisp-format py-yapf lsp-pyright terraform-mode swiper hcl-mode
												  company glsl-mode lsp-mode typescript-mode
												  json-mode flycheck evil-numbers yasnippet
												  rust-mode yaml-mode web-mode vue-mode toml-mode
												  protobuf-mode php-mode nixos-options nix-mode
												  lsp-ui haskell-mode go-mode git-blamed
												  editorconfig dockerfile-mode dart-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(terraform--resource-name-face ((t 
								   (:foreground "brightmagenta")))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(terraform--resource-name-face ((t (:foreground "brightmagenta")))))
