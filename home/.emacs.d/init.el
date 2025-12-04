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

(straight-use-package 'magit)

(straight-use-package 'yasnippet)
(yas-global-mode 1)

(straight-use-package 'editorconfig)
(editorconfig-mode 1)

(straight-use-package 'tree-sitter)

(require 'eglot)
(require 'project)
(require 'cl-lib)
(defun project-try-terraform (dir)
  "Return Terraform project if a nearest `.terraform/` is found above DIR."
  (when-let ((root (locate-dominating-file dir ".terraform")))
    (cons 'terraform root)))
(defun project-try-cargo (dir)
  "Return Cargo project if a nearest `Cargo.toml` is found above DIR."
  (when-let ((root (locate-dominating-file dir "Cargo.toml")))
    (cons 'cargo root)))
(cl-defmethod project-root ((project (head terraform)))
  (cdr project))
(cl-defmethod project-root ((project (head cargo)))
  (cdr project))
(add-hook 'terraform-mode-hook
          (lambda ()
            (add-hook 'project-find-functions #'project-try-terraform nil t)))
(add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'project-find-functions #'project-try-cargo nil t)))

;; use eglot for these languages
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'sh-mode-hook #'eglot-ensure)
(add-hook 'dockerfile-mode-hook #'eglot-ensure)
(add-hook 'c-mode-common-hook #'eglot-ensure)
(add-hook 'terraform-mode-hook #'eglot-ensure)
(add-hook 'mhtml-mode-hook #'eglot-ensure)
(add-hook 'typescript-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)
(add-hook 'typscript-ts-mode-hook #'eglot-ensure)
(add-hook 'js-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(add-hook 'yaml-mode-hook #'eglot-ensure)
(add-hook 'yaml-ts-mode-hook #'eglot-ensure)

;; eglot configuration
(setq eglot-autoshutdown t) ;; shutdown language server after closing last file
(setq eglot-confirm-server-initiated-edits nil) ;; don't ask for confirmation on server edits
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

(straight-use-package 'apheleia)
(require 'apheleia)
(defun declare-formatter (name cmd)
  "Set apheleia formatter NAME to use command CMD."
  (push (cons name cmd) apheleia-formatters))
(declare-formatter 'clang-format '("clang-format" "-"))
(declare-formatter 'clang-format-protobuf '("clang-format" "--assume-filename=.proto" "-"))
(declare-formatter 'jq '("jq" "."))
(declare-formatter 'justfile '("sed" "s/\t/    /g"))
(declare-formatter 'nixfmt '("nixfmt" "-"))
(declare-formatter 'prettier '("prettier" "--stdin-filepath" filepath))
(declare-formatter 'python-fmt '("fmt-python"))
(declare-formatter 'shfmt '("beautysh" "-"))
(declare-formatter 'taplo '("taplo" "fmt" "-"))
(declare-formatter 'terraform-fmt '("terraform" "fmt" "-"))
(declare-formatter 'rustfmt '("rustfmt" "--edition" "2024"))

(defun assign-formatter (mode formatter)
  (setf (alist-get mode apheleia-mode-alist) (list formatter)))
(assign-formatter 'c++-mode 'clang-format)
(assign-formatter 'c-mode 'clang-format)
(assign-formatter 'c++-ts-mode 'clang-format)
(assign-formatter 'c-ts-mode 'clang-format)
(assign-formatter 'html-mode 'prettier)
(assign-formatter 'js-json-mode 'prettier)
(assign-formatter 'js-mode 'prettier)
(assign-formatter 'json-mode 'prettier)
(assign-formatter 'just-mode 'justfile)
(assign-formatter 'nix-mode 'nixfmt)
(assign-formatter 'protobuf-mode 'clang-format-protobuf)
(assign-formatter 'python-mode 'python-fmt)
(assign-formatter 'python-ts-mode 'python-fmt)
(assign-formatter 'sh-mode 'shfmt)
(assign-formatter 'terraform-mode 'terraform-fmt)
(assign-formatter 'toml-mode 'taplo)
(assign-formatter 'ts-json-mode 'prettier)
(assign-formatter 'tsx-mode 'prettier)
(assign-formatter 'tsx-ts-mode 'prettier)
(assign-formatter 'typescript-mode 'prettier)
(assign-formatter 'typescript-ts-mode 'prettier)
(assign-formatter 'yaml-mode 'prettier)
(assign-formatter 'yaml-ts-mode 'prettier)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(straight-use-package 'ivy)
(ivy-mode)

(straight-use-package 'swiper)

(straight-use-package 'company)
(setq company-tooltip-align-annotations t)
(setq company-minimum-prefix-length 1000) ;; don't offer autocomplete unless "C-c l" is pressed
(global-company-mode)

(straight-use-package
 '(copilot :type git
           :host github
           :repo "zerolfx/copilot.el"
           :files ("dist" "*.el")))
(require 'copilot)
(setq copilot-indent-offset-warning-disable t)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(straight-use-package 'rust-mode)
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
(straight-use-package 'haskell-mode)
(straight-use-package 'go-mode)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'nix-mode)

(straight-use-package 'markdown-mode) ;; eglot uses markdown-mode to render pretty docs
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode)))

(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))

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

(defun explainshell (start end)
  "Open explainshell.com to explain the selected region."
  (interactive "r")
  (let ((selected-text (buffer-substring-no-properties start end)))
    (browse-url
     (concat "https://explainshell.com/explain?cmd="
             (url-hexify-string selected-text)))))

;; wish: port https://codeberg.org/ideasman42/emacs-elisp-autofmt to rust or pure lisp
(defun fmt-elisp-buffer ()
  "Format the current buffer as Emacs Lisp code."
  (interactive)
  (indent-region (point-min) (point-max)))

(defvar bddap-keymap (make-sparse-keymap) "reserved bindings that override everything")
(define-minor-mode bddap-keymap-mode
  "Prioritize my own bindings."
  :global t :init-value t)
(add-to-list 'emulation-mode-map-alists `((bddap-keymap-mode . ,bddap-keymap)))
(defun bddap-bind (keys fn)
  "Bind FN on KEYS in a high-priority map."
  (define-key bddap-keymap (kbd keys) fn))
(bddap-bind "M--" 'decrement-number-at-point)
(bddap-bind "M-=" 'increment-number-at-point)
(bddap-bind "C-c f" 'project-find-file)
(bddap-bind "C-c C-l" 'copilot-accept-completion)
(bddap-bind "C-c C-k" 'copilot-accept-completion-by-line)
(bddap-bind "M-a" 'copilot-accept-completion-by-word)
(bddap-bind "C-c C-o" 'copilot-mode)
(bddap-bind "C-c e" 'explainshell)
(bddap-bind "C-c C-f" 'apheleia-format-buffer)
(bddap-bind "C-c C-g" 'fmt-elisp-buffer)
(bddap-bind "C-c g" 'googleit)
(bddap-bind "C-c i" 'xref-find-references)
(bddap-bind "C-c l" 'company-complete-common)
(bddap-bind "C-c n e" 'next-err)
(bddap-bind "C-c o" 'eldoc-doc-buffer)
(bddap-bind "C-c r" 'refac)
(bddap-bind "C-c u" 'eglot-rename)
(bddap-bind "C-c y" 'eglot-code-actions)
(bddap-bind "M-." 'xref-find-definitions)
(bddap-bind "M-n" 'flymake-goto-next-error)
(bddap-bind "M-p" 'flymake-goto-prev-error)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful. Your init file should
 ;; contain only one such instance. If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful. Your init file should
 ;; contain only one such instance. If there is more than one, they won't work right.
 '(terraform--resource-name-face ((t (:foreground "yellow"))))
 '(eglot-inlay-hint-face ((t (:foreground "gray32"))))
 )
