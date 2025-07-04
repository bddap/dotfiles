;; -*- lexical-binding: t -*-

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "saves/bak/" user-emacs-directory))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "saves/auto/" user-emacs-directory) t)))

;; stop emacs from creating '.#filename' files because they mess with filesystem watchers
(setq create-lockfiles nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(column-number-mode 1)
(menu-bar-mode -1)
(setq tab-always-indent t) ;; don't use tab for autocomplete
(global-set-key (kbd "C-c f") 'project-find-file)

(straight-use-package 'magit)

(straight-use-package 'yasnippet)
(yas-global-mode 1)

(straight-use-package 'editorconfig)
(editorconfig-mode 1)

(straight-use-package 'tree-sitter)

(straight-use-package 'lsp-mode)
(require 'lsp)
(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("terraform-ls"))
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
(add-hook 'typescript-ts-mode-hook #'lsp)
(add-hook 'tsx-ts-mode-hook #'lsp)
(add-hook 'typscript-ts-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'nix-mode-hook #'lsp)
(add-hook 'python-ts-mode-hook #'lsp)
;; to use the lsp whenever possible:
;; (add-hook 'prog-mode-hook #'lsp)

;; (setq major-mode-remap-alist
;;       '((bash-mode        . bash-ts-mode)
;;         (c-mode           . c-ts-mode)
;;         (c++-mode         . c++-ts-mode)
;;         (css-mode         . css-ts-mode)
;;         (javascript-mode  . js-ts-mode)
;;         (js-json-mode     . json-ts-mode)
;;         (json-mode        . json-ts-mode)
;;         (python-mode      . python-ts-mode)
;;         (ruby-mode        . ruby-ts-mode)
;;         (typescript-mode  . typescript-ts-mode)
;;         (tsx-mode         . tsx-ts-mode)
;;         (yaml-mode        . yaml-ts-mode)))

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
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-doc-enable nil)
  (setq lsp-signature-render-documentation nil))
(setq lsp-prefer-flymake nil) ;; tell lsp to use flycheck instead
;; (setq lsp-inlay-hint-enable t)

(straight-use-package 'lsp-ui)
(add-hook 'lsp-ui-mode-hook 'lsp-ui-doc-hide)

(straight-use-package 'apheleia)
(require 'apheleia)
;; (push '(black . ("black" "--stdin-filename" filepath "-")) apheleia-formatters)
;; (push '(isort . ("isort" "--stdout" "--profile" "black" "-")) apheleia-formatters)
;; (push '(add-trailing-comma . ("add-trailing-comma" "--py36-plus" "--exit-zero-even-if-changed" "-")) apheleia-formatters)
(push
 '(python-fmt . ("bash" "-c" "ruff format - | isort --stdout --profile black -"))
 apheleia-formatters)
(push '(jq . ("jq" ".")) apheleia-formatters)
(push '(prettier . ("prettier" "--stdin-filepath" filepath)) apheleia-formatters)
(push '(shfmt . ("beautysh" "-")) apheleia-formatters)
(push
 '(clang-format-protobuf
   . ("clang-format" "--assume-filename=.proto" "-"))
 apheleia-formatters)
(push '(clang-format . ("clang-format" "-")) apheleia-formatters)
(push '(justfile . ("sed" "s/\t/    /g")) apheleia-formatters)
(push '(taplo . ("taplo" "fmt" "-")) apheleia-formatters)

;; this function relies on lexical-binding, which is off by default. It is enabled at the top of this file
(defun use-apheleia (hook formatter)
  (add-hook hook
            (lambda ()
              (progn
                (define-key lsp-mode-map (kbd "C-c C-f") nil)
                (local-set-key (kbd "C-c C-f")
                               (lambda ()
                                 (interactive)
                                 (apheleia-format-buffer formatter)))))))

(use-apheleia 'python-mode-hook 'python-fmt)
(use-apheleia 'json-mode-hook 'prettier)
(use-apheleia 'js-json-mode-hook 'prettier)
(use-apheleia 'ts-json-mode-hook 'prettier)
(use-apheleia 'js-mode-hook 'prettier)
(use-apheleia 'yaml-mode-hook 'prettier)
(use-apheleia 'typescript-mode-hook 'prettier)
(use-apheleia 'typescript-ts-mode-hook 'prettier)
(use-apheleia 'tsx-ts-mode-hook 'prettier)
(use-apheleia 'tsx-mode-hook 'prettier)
(use-apheleia 'html-mode-hook 'prettier)
(use-apheleia 'sh-mode-hook 'shfmt)
(use-apheleia 'c-mode-hook 'clang-format)
(use-apheleia 'protobuf-mode-hook 'clang-format-protobuf)
(use-apheleia 'just-mode-hook 'justfile)
(use-apheleia 'toml-mode-hook 'taplo)

;; todo: port https://codeberg.org/ideasman42/emacs-elisp-autofmt to rust or pure lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-f")
                           (lambda ()
                             (interactive)
                             (indent-region (point-min) (point-max))))))

(straight-use-package 'lsp-pyright)

(straight-use-package 'rust-mode)
;; https://github.com/rust-lang/rust-mode#indentation
(add-hook 'rust-mode-hook
          (lambda ()
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

(straight-use-package
 '(copilot :type git
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

(define-key copilot-completion-map (kbd "C-c C-l")
			'copilot-accept-completion)
(define-key copilot-completion-map (kbd "M-p")
			'copilot-previous-completion)
(global-unset-key (kbd "M-i"))
(define-key copilot-completion-map (kbd "M-i")
			'copilot-next-completion)
(define-key copilot-completion-map (kbd "M-j")
			'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "M-n")
			'copilot-accept-completion-by-line)

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
(add-hook 'js-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'lsp-goto-implementation)))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-ts-mode))

;; (require 'use-package)

;; terraform-lsp has no formatting provider. luckily terraform-mode can do formatting
(add-hook 'terraform-mode-hook
          (lambda ()
            (progn
              (define-key lsp-mode-map (kbd "C-c C-f") nil)
              (local-set-key (kbd "C-c C-f") #'terraform-format-buffer))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode)))

(defun increment-number-at-point ()
  (interactive)
  (let ((old-point (point)))
    (unwind-protect
        (progn
          (skip-chars-backward "0-9")
          (or (looking-at "\\-?[0-9]+")
              (error "No number at point"))
          (replace-match
           (number-to-string
            (1+ (string-to-number (match-string 0))))))
      (goto-char old-point))))
(global-set-key (kbd "\C-c =") 'increment-number-at-point)

(defun decrement-number-at-point ()
  (interactive)
  (let ((old-point (point)))
    (unwind-protect
        (progn
          (skip-chars-backward "0-9")
          (or (looking-at "\\-?[0-9]+")
              (error "No number at point"))
          (replace-match
           (number-to-string
            (1- (string-to-number (match-string 0))))))
      (goto-char old-point))))
(global-set-key (kbd "\C-c -") 'decrement-number-at-point)

(defun jump-to-file-char (path charnum)
  "Open existing file at PATH and move point to CHARNUM."
  (interactive)
  (find-file-existing path)
  (goto-char charnum))

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process shell-file-name nil t nil shell-command-switch command))))

(require 'json)
(defun get-next-err ()
  "Determine the file-name and character location of the next rustc error."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (js (json-read-from-string
              (shell-command-to-string "next-rustc-err"))))
    (list (gethash "file_name" js)
          (gethash "byte_start" js))))

(defun next-err ()
  "Go to location of next compile error."
  (interactive)
  (let ((ner (get-next-err)))
    (jump-to-file-char (nth 0 ner) (1+ (nth 1 ner)))))

(defun refac-call-executable-async (selected-text transform callback)
  "Asynchronously call the refac executable, passing SELECTED-TEXT and TRANSFORM.
CALLBACK is a function taking two parameters: EXIT-STATUS and RESULT."
  (let ((refac-executable (executable-find "refac"))
        (temp-buf (generate-new-buffer "*refac-output*")))
    (if refac-executable
        (make-process
         :name "refac-async"
         :buffer temp-buf
         :command (list refac-executable "tor" selected-text transform)
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let ((buf (process-buffer proc)))
               ;; Ensure the buffer is still alive before attempting to read from it
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((exit-status (process-exit-status proc))
                         (result (buffer-string)))
                     (kill-buffer buf)
                     (funcall callback exit-status result))))))))
      (error "refac executable not found"))))

(global-set-key (kbd "C-c r") 'refac)

(defun refac (beg end transform)
  "Perform the refac transform in place, inserting merge markers inline, asynchronously.
The overlay will display the transform prompt until the results arrive."
  (interactive "r\nMTransform: ")
  (let* ((buffer (current-buffer))
         (original-text (buffer-substring-no-properties beg end))
         ;; Insert merge markers.
         (_ (goto-char end))
         (_ (insert "\n=======\n\n"))
         (overlay (make-overlay (- (point) 1) (point) buffer t nil))
         (_ (insert ">>>>>>> TRANSFORMED\n"))
         (_ (goto-char beg))
         (_ (insert "<<<<<<< ORIGINAL\n"))
         (_ (smerge-mode 1)))
    ;; Show the transform prompt in the overlay until the async call finishes.
    (overlay-put overlay 'display
                 (concat "Running refac...\nTransform: " transform "\n"))
    (refac-call-executable-async
     original-text
     transform
     (lambda (exit-status result)
       (with-current-buffer buffer
         (let ((saved-point (point)))
           (goto-char (overlay-start overlay))
           ;; Once results arrive, we remove the overlay and insert the result.
           (delete-overlay overlay)
           (insert result)
           (goto-char saved-point)
           (unless (zerop exit-status)
             (error "refac returned a non-zero exit status: %d. Error: %s"
                    exit-status result))))))))

(require 'url-util)

(defun googleit (start end)
  "Search selected region with Google."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties start end)))
    (browse-url
     (concat "https://www.google.com/search?q="
             (url-hexify-string selected-text)))))

(global-set-key (kbd "C-c g") 'googleit)

(global-set-key (kbd "C-c w") 'refac-inline)
(global-set-key (kbd "C-c q") 'refac-smerge)

(defun refac-smerge (beg end transform)
  "Perform the REFAC transform in place, inserting merge markers inline."
  (interactive "r\nMTransform: ")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (new-text (refac-call-executable original-text transform)))
    ;; Remove the selected region, replacing it with conflict markers.
    (goto-char end)
    (insert "\n=======\n")
    (insert new-text)
    (insert "\n>>>>>>> TRANSFORMED\n")
    (goto-char beg)
    (insert "<<<<<<< ORIGINAL\n")
    ;; Enable smerge-mode for conflict resolution right in the buffer.
    (smerge-mode 1)))

(defun refac-git-style-diff (a b)
  "Return a Git-style unified diff between strings A and B."
  (with-temp-buffer
    (let ((temp-file-a (make-temp-file "a"))
          (temp-file-b (make-temp-file "b")))
      (unwind-protect
          (progn
            (write-region a nil temp-file-a)
            (write-region b nil temp-file-b)
            (call-process "diff" nil t nil "-u" temp-file-a temp-file-b)
            (buffer-string))
        (delete-file temp-file-a)
        (delete-file temp-file-b)))))

(defun refac-inline (beg end transform)
  "Call `refac-call-executable' on the contents of selected region using TRANSFORM, then show a split diff.
Prompt to accept or deny the changes."
  (interactive "r\nMTransform: ")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (new-text (refac-call-executable original-text transform))
         (diff-text (refac-git-style-diff original-text new-text))
         (overlay (make-overlay beg end (current-buffer) t nil)))
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'display diff-text)
    (message "Proposed changes. Press (a)ccept or (b)deny.")
    (let ((response (read-char-exclusive)))
      (cond
       ((eq response ?a)
        ;; Accept changes: replace original with new text
        (delete-overlay overlay)
        (delete-region beg end)
        (goto-char beg)
        (insert new-text)
        (message "Changes accepted."))
       (t
        ;; Deny changes: revert to original
        (delete-region beg (+ beg (length diff-text)))
        (goto-char beg)
        (insert original-text)
        (overlay-put overlay 'face nil)
        (delete-overlay overlay)
        (message "Changes denied."))))))

(defun refac-call-executable (selected-text transform)
  "Synchronously call the refac executable, passing SELECTED-TEXT and TRANSFORM."
  (let (result exit-status refac-executable)
    (setq refac-executable (executable-find "refac"))
    (if refac-executable
        (with-temp-buffer
          (setq exit-status
                (call-process refac-executable nil t nil "tor" selected-text transform))
          (setq result (buffer-string)))
      (error "refac executable not found"))
    (if (zerop exit-status)
        result
      (error "refac returned a non-zero exit status: %d. Error: %s"
             exit-status result))))

;; lookup snippet in explainshell.com
(defun explainshell (start end)
  "Open explainshell.com with the selected region."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties start end)))
    (browse-url
     (concat "https://explainshell.com/explain?cmd="
             (url-hexify-string selected-text)))))

(global-set-key (kbd "C-c e") 'explainshell)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful. Your init file should
 ;; contain only one such instance. If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful. Your init file should
 ;; contain only one such instance. If there is more than one, they won't work right.
 '(terraform--resource-name-face ((t (:foreground "yellow")))))
