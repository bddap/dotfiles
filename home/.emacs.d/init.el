;; -*- lexical-binding: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)) 
(bootstrap-version 6)) 
(unless (file-exists-p bootstrap-file) 
(with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el" 'silent 'inhibit-cookies) 
(goto-char (point-max)) 
(eval-print-last-sexp))) 
(load bootstrap-file nil 'nomessage))

(setq backup-directory-alist `(("." . ,(expand-file-name "saves/bak/" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "saves/auto/" user-emacs-directory) t)))
;; stop emacs from creating '.#filename' files becuase they mess with filesystem watchers
(setq create-lockfiles nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(column-number-mode 1)
(menu-bar-mode -1)
(setq tab-always-indent t) ;; don't use tab for autocomplete
(global-set-key (kbd "C-c f") 'project-find-file)

(straight-use-package 'yasnippet)
(yas-global-mode 1)

(straight-use-package 'editorconfig)
(editorconfig-mode 1)

(straight-use-package 'lsp-mode)
(require 'lsp)
(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
(lsp-register-client (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls")) 
:major-modes '(terraform-mode) 
:server-id 'terraform-ls))

;; use lsp for these languages
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'sh-mode-hook #'lsp)
(add-hook 'dockerfile-mode-hook #'lsp)
(add-hook 'c-mode-common-hook #'lsp)
(add-hook 'terraform-mode-hook #'lsp)
(add-hook 'mhtml-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'nix-mode-hook #'lsp)
;; to use the lsp whenever possible:
;; (add-hook 'prog-mode-hook #'lsp)
(define-key lsp-mode-map (kbd "C-c u") #'lsp-rename)
(define-key lsp-mode-map (kbd "C-c i") #'lsp-ui-peek-find-references)
(define-key lsp-mode-map (kbd "C-c o") #'lsp-ui-doc-glance)
(define-key lsp-mode-map (kbd "C-c y") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "M-n") #'flycheck-next-error)
(define-key lsp-mode-map (kbd "M-p") #'flycheck-previous-error)
(define-key lsp-mode-map (kbd "C-c h") #'flycheck-first-error)
(define-key lsp-mode-map (kbd "C-c C-f") #'lsp-format-buffer)
;; keep lsp-ui-doc from popping up without my say-so
(setq lsp-ui-doc-enable nil)
(setq lsp-file-watch-threshold 100000)
(with-eval-after-load 'lsp-mode (setq lsp-ui-doc-enable nil) 
(setq lsp-signature-render-documentation nil))
(setq lsp-prefer-flymake nil) ;; tell lsp to use flycheck instead
;; (setq lsp-inlay-hint-enable t)

(straight-use-package 'lsp-ui)
(add-hook 'lsp-ui-mode-hook 'lsp-ui-doc-hide)

(straight-use-package 'apheleia)
(require 'apheleia)
;; (push '(black . ("black" "--stdin-filename" filepath "-")) apheleia-formatters)
;; (push '(isort . ("isort" "--stdout" "--profile" "black" "-")) apheleia-formatters)
;; (push '(add-trailing-comma . ("add-trailing-comma" "--py36-plus" "--exit-zero-even-if-changed" "-"))
;; 	  apheleia-formatters)
(push '(python-fmt . ("bash" "-c" "ruff format - | isort --stdout --profile black -")) apheleia-formatters)
(push '(jq . ("jq" ".")) apheleia-formatters)
(push '(prettier . ("prettier" "--stdin-filepath" filepath)) apheleia-formatters)
(push '(shfmt . ("beautysh" "-")) apheleia-formatters)
(push '(clang-format-protobuf . ("clang-format" "--assume-filename=.proto" "-")) apheleia-formatters)
(push '(clang-format . ("clang-format" "-")) apheleia-formatters)
(push '(justfile . ("sed" "s/\t/    /g")) apheleia-formatters)
(push '(taplo . ("taplo" "fmt" "-")) apheleia-formatters)
;; this function relies on lexical-binding, which is off by default. It is enabled at the top of this file
(defun use-apheleia (hook formatter) 
(add-hook hook (lambda () 
(progn (define-key lsp-mode-map (kbd "C-c C-f") nil) 
(local-set-key (kbd "C-c C-f") 
(lambda () 
(interactive) 
(apheleia-format-buffer formatter)))))))
(use-apheleia 'python-mode-hook 'python-fmt)
(use-apheleia 'json-mode-hook 'prettier)
(use-apheleia 'js-mode-hook 'prettier)
(use-apheleia 'yaml-mode-hook 'prettier)
(use-apheleia 'typescript-mode-hook 'prettier)
(use-apheleia 'html-mode-hook 'prettier)
(use-apheleia 'sh-mode-hook 'shfmt)
(use-apheleia 'c-mode-hook 'clang-format)
(use-apheleia 'protobuf-mode-hook 'clang-format-protobuf)
(use-apheleia 'just-mode-hook 'justfile)
(use-apheleia 'toml-mode-hook 'taplo)


(straight-use-package 'elisp-format)
(add-hook 'emacs-lisp-mode-hook (lambda () 
(local-set-key (kbd "C-c C-f") #'elisp-format-buffer)))
(setq elisp-format-column 120) ;; https://github.com/Yuki-Inoue/elisp-format/issues/3

(straight-use-package 'lsp-pyright)

(straight-use-package 'rust-mode)
;; https://github.com/rust-lang/rust-mode#indentation
(add-hook 'rust-mode-hook (lambda () 
(setq indent-tabs-mode nil) 
(local-set-key (kbd "\C-x n e") 'next-err)))
;; https://users.rust-lang.org/t/how-to-disable-rust-analyzer-proc-macro-warnings-in-neovim/53150/8
(setq lsp-rust-analyzer-proc-macro-enable t)
(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
(setq lsp-rust-analyzer-experimental-proc-attr-macros t)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")
;; (defun bddap-lsp-config ()
;;   "set lsp config"
;;   (interactive)
;;   (lsp--set-configuration `(:rust-analyzer (:cargo (:allFeatures (true))))))

(setq lsp-nix-nil-formatter ["nixfmt"])

(straight-use-package 'ivy)
(ivy-mode)

(straight-use-package 'swiper)
(global-set-key (kbd "\C-c C-s") 'swiper-isearch)
(global-set-key (kbd "\C-c C-r") 'swiper-isearch-reverse)

(straight-use-package 'company)
(setq company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 1000) ;; don't offer autocomplete unless "C-c l" is pressed
(global-set-key (kbd "C-c l") 'company-complete-common)

(straight-use-package '(copilot :type git 
:host github 
:repo "zerolfx/copilot.el" 
:files ("dist" "*.el")))
(require 'copilot)
(setq copilot-indent-offset-warning-disable t)

;; enable copilot by default
;; (global-copilot-mode 1)
;; key to toggle copilot
(global-set-key (kbd "C-c C-o") 'copilot-mode)
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
(global-set-key (kbd "C-c C-l") 
(lambda () 
(copilot-complete 1)))
(define-key copilot-completion-map (kbd "C-c C-l") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion)
(global-unset-key (kbd "M-i"))
(define-key copilot-completion-map (kbd "M-i") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "M-j") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "M-n") 'copilot-accept-completion-by-line)

(straight-use-package 'just-mode)
(straight-use-package 'rainbow-mode)
(straight-use-package 'terraform-mode)
(straight-use-package 'hcl-mode)
(straight-use-package 'glsl-mode)
(straight-use-package 'typescript-mode)
(straight-use-package 'flycheck)
(straight-use-package 'yaml-mode)
(straight-use-package 'web-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'protobuf-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'haskell-mode)
(straight-use-package 'go-mode)
(straight-use-package 'dockerfile-mode)

(straight-use-package 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; js-mode binds "M-." to js-find-symbol. We don't want that because lsp-goto-implementation is better.
(add-hook 'js-mode-hook (lambda () 
(local-set-key (kbd "M-.") 'lsp-goto-implementation)))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; (require 'use-package)

;; terraform-lsp has no formatting provider. luckily terraform-mode can do formatting
(add-hook 'terraform-mode-hook (lambda () 
(progn (define-key lsp-mode-map (kbd "C-c C-f") nil) 
(local-set-key (kbd "C-c C-f") #'terraform-format-buffer))))

(add-hook 'markdown-mode-hook (lambda () 
(visual-line-mode)))

(defun increment-number-at-point () 
(interactive) 
(let ((old-point (point))) 
(unwind-protect (progn (skip-chars-backward "0-9") 
(or (looking-at "\-?[0-9]+") 
(error "No number at point")) 
(replace-match (number-to-string (1+ (string-to-number (match-string 0)))))) 
(goto-char old-point))))
(global-set-key (kbd "\C-c =") 'increment-number-at-point)

(defun decrement-number-at-point () 
(interactive) 
(let ((old-point (point))) 
(unwind-protect (progn (skip-chars-backward "0-9") 
(or (looking-at "\-?[0-9]+") 
(error "No number at point")) 
(replace-match (number-to-string (1- (string-to-number (match-string 0)))))) 
(goto-char old-point))))
(global-set-key (kbd "\C-c -") 'decrement-number-at-point)

(defun jump-to-file-char (path charnum) "open existing file at line" (interactive) 
(find-file-existing path) 
(goto-char charnum))

(defun shell-command-to-string (command) "Execute shell command COMMAND and return its output as a string." (with-output-to-string (with-current-buffer standard-output (call-process shell-file-name nil t nil shell-command-switch command))))
(require 'json)
(defun get-next-err () "determine the file-name and charater location of the next rustc error" (let* ((json-object-type 'hash-table) 
(json-array-type 'list) 
(json-key-type 'string) 
(js (json-read-from-string (shell-command-to-string "next-rustc-err"))) 
(noth (type-of (gethash "byte_start" js)))) 
(list (gethash "file_name" js) 
(gethash "byte_start" js))))

(defun next-err () "go to location of next compile error" (interactive) 
(let ((ner (get-next-err))) 
(jump-to-file-char (nth 0 ner) 
(+ 1 (nth 1 ner)))))

(defun refac-git-style-diff (a b) 
(with-temp-buffer (let ((temp-file-a (make-temp-file "a")) 
(temp-file-b (make-temp-file "b"))) 
(unwind-protect (progn (write-region a nil temp-file-a) 
(write-region b nil temp-file-b) 
(call-process "diff" nil t nil "-u" temp-file-a temp-file-b) 
(buffer-string)) 
(delete-file temp-file-a) 
(delete-file temp-file-b)))))

(defun refac-filter-diff-output (diff-output) 
(with-temp-buffer (insert diff-output) 
(goto-char (point-min)) 
(while (not (eobp)) 
(let ((line (buffer-substring-no-properties (line-beginning-position) 
(line-end-position)))) 
(if (or (string-prefix-p "--- " line) 
(string-prefix-p "+++ " line) 
(string-prefix-p "\\ No newline at end of file" line)) 
(delete-region (line-beginning-position) 
(1+ (line-end-position))) 
(forward-line)))) 
(buffer-string)))


(defun refac-call-executable (selected-text transform) 
(let (result exit-status refac-executable) 
(setq refac-executable (executable-find "refac")) 
(if refac-executable (with-temp-buffer (setq exit-status (call-process refac-executable nil t nil "tor" selected-text transform)) 
(setq result (buffer-string))) 
(error "refac executable not found")) 
(if (zerop exit-status) result (error "refac returned a non-zero exit status: %d. Error: %s" exit-status result))))

(defun refac (start end) 
(interactive "r") 
(let* ((selected-text (buffer-substring-no-properties start end)) 
(transform (read-string "Enter transformation instruction: "))) 
(let ((result (refac-call-executable selected-text transform))) 
(delete-region start end) 
(insert result) 
(let ((diff-output (refac-git-style-diff selected-text result))) 
(message (refac-filter-diff-output diff-output))))))

(global-set-key (kbd "C-c r") 'refac)

(require 'url-util)
(defun googleit (start end) 
(interactive "r") 
(let ((selected-text (buffer-substring-no-properties start end))) 
(browse-url (concat "https://www.google.com/search?q=" (url-hexify-string selected-text)))))
(global-set-key (kbd "C-c g") 'googleit)

;; lookup snippent in explainshell.com
(defun explainshell (start end) 
(interactive "r") 
(let ((selected-text (buffer-substring-no-properties start end))) 
(browse-url (concat "https://explainshell.com/explain?cmd=" (url-hexify-string selected-text)))))
(global-set-key (kbd "C-c e") 'explainshell)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(terraform--resource-name-face ((t (:foreground "yellow")))))
