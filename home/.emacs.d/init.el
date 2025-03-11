;; -*- lexical-binding: t -*-

(defvar bootstrap-version)
(let
	((bootstrap-file
	  (expand-file-name "straight/repos/straight.el/bootstrap.el"
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
	  `
	  ((".*" ,(expand-file-name "saves/auto/" user-emacs-directory) t)))
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
(add-to-list 'lsp-language-id-configuration
			 '(terraform-mode . "terraform"))
(lsp-register-client (make-lsp-client :new-connection
									  (lsp-stdio-connection
									   '("terraform-ls")) 
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
;; (push '(add-trailing-comma . ("add-trailing-comma" "--py36-plus" "--exit-zero-even-if-changed" "-"))
;; 	  apheleia-formatters)
(push
 '(python-fmt
   . ("bash" "-c" "ruff format - | isort --stdout --profile black -"))
 apheleia-formatters)
(push '(jq . ("jq" ".")) apheleia-formatters)
(push '(prettier . ("prettier" "--stdin-filepath" filepath))
	  apheleia-formatters)
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
  (add-hook hook (lambda () 
				   (progn
					 (define-key lsp-mode-map (kbd "C-c C-f") nil) 
					 (local-set-key (kbd "C-c C-f") 
									(lambda () 
									  (interactive) 
									  (apheleia-format-buffer
									   formatter)))))))
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
(add-hook 'js-mode-hook (lambda () 
						  (local-set-key (kbd "M-.")
										 'lsp-goto-implementation)))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; (require 'use-package)

;; terraform-lsp has no formatting provider. luckily terraform-mode can do formatting
(add-hook 'terraform-mode-hook (lambda () 
								 (progn
								   (define-key lsp-mode-map
											   (kbd "C-c C-f") nil) 
								   (local-set-key (kbd "C-c C-f")
												  #'terraform-format-buffer))))

(add-hook 'markdown-mode-hook (lambda () 
								(visual-line-mode)))

(defun increment-number-at-point () 
  (interactive) 
  (let ((old-point (point))) 
	(unwind-protect (progn
					  (skip-chars-backward "0-9") 
					  (or (looking-at "\-?[0-9]+") 
						  (error "No number at point")) 
					  (replace-match
					   (number-to-string
						(1+ (string-to-number (match-string 0)))))) 
	  (goto-char old-point))))
(global-set-key (kbd "\C-c =") 'increment-number-at-point)

(defun decrement-number-at-point () 
  (interactive) 
  (let ((old-point (point))) 
	(unwind-protect (progn
					  (skip-chars-backward "0-9") 
					  (or (looking-at "\-?[0-9]+") 
						  (error "No number at point")) 
					  (replace-match
					   (number-to-string
						(1- (string-to-number (match-string 0)))))) 
	  (goto-char old-point))))
(global-set-key (kbd "\C-c -") 'decrement-number-at-point)

(defun jump-to-file-char (path charnum)
  "open existing file at line" (interactive) 
  (find-file-existing path) 
  (goto-char charnum))

(defun shell-command-to-string (command)
  "Execute shell command COMMAND and return its output as a string."
  (with-output-to-string
	(with-current-buffer standard-output
	  (call-process shell-file-name nil t nil shell-command-switch
					command))))
(require 'json)
(defun get-next-err ()
  "determine the file-name and charater location of the next rustc error"
  (let* ((json-object-type 'hash-table)
		 
		 (json-array-type
		  'list) 
		 (json-key-type
		  'string) 
		 (js
		  (json-read-from-string
		   (shell-command-to-string
			"next-rustc-err"))) 
		 (noth
		  (type-of
		   (gethash
			"byte_start"
			js)))) 
	(list
	 (gethash
	  "file_name"
	  js) 
	 (gethash
	  "byte_start"
	  js))))

(defun next-err ()
  "go to location of next compile error" (interactive) 
  (let ((ner (get-next-err))) 
	(jump-to-file-char (nth 0 ner) 
					   (+ 1 (nth 1 ner)))))


(defun refac-call-executable-async (selected-text transform callback)
  "Asynchronously call the refac executable, passing SELECTED-TEXT and TRANSFORM as arguments. 
CALLBACK is a function taking two parameters: EXIT-STATUS and RESULT."
  (let ((refac-executable (executable-find "refac"))
        (temp-buf (generate-new-buffer "*refac-output*")))
    (if refac-executable
        (make-process
         :name "refac-async"
         :buffer temp-buf
         :command (list refac-executable "tor" selected-text transform)
         :sentinel (lambda (proc _event)
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
  "Perform the refac transform in place, inserting merge markers inline, asynchronously."
  (interactive "r\nMTransform: ")
  (let* ((buffer (current-buffer))
         (original-text (buffer-substring-no-properties beg end))
         ;; Insert merge markers
         (_ (goto-char end))
         (_ (insert "\n=======\n"))
         (overlay (make-overlay (point) (point) buffer t nil))
         (_ (insert ">>>>>>> TRANSFORMED\n"))
         (_ (goto-char beg))
         (_ (insert "<<<<<<< ORIGINAL\n"))
         (_ (smerge-mode 1)))
	(overlay-put overlay 'display (concat "Running refac...\nTransform: " transform "\n"))
    (refac-call-executable-async
     original-text
     transform
     (lambda (exit-status result)
       (with-current-buffer buffer
         (let ((saved-point (point)))
           (goto-char (overlay-start overlay))
           (delete-overlay overlay)
           (insert result)
           (goto-char saved-point)
           (unless (zerop exit-status)
             (error "refac returned a non-zero exit status: %d. Error: %s"
                    exit-status result))))))))

(require 'url-util)

(defun googleit (start end)
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
  (with-temp-buffer
	(let ((temp-file-a (make-temp-file "a")) 
		  (temp-file-b (make-temp-file "b"))) 
	  (unwind-protect (progn
						(write-region a nil
									  temp-file-a) 
						(write-region b nil
									  temp-file-b) 
						(call-process "diff" nil
									  t nil "-u"
									  temp-file-a
									  temp-file-b) 
						(buffer-string)) 
		(delete-file temp-file-a) 
		(delete-file temp-file-b)))))

(defun refac-inline (beg end transform)
  "Call `refac-call-executable' on the contents of selected region using TRANSFORM.
Instead of in-place replacement, create an overlay with a split
showing the original text on the left and transformed text on the
right in a minimal GitHub-style diff. Prompt to accept or deny the
changes."
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
  (let (result exit-status refac-executable) 
	(setq refac-executable (executable-find "refac")) 
	(if refac-executable (with-temp-buffer
						   (setq exit-status
								 (call-process refac-executable nil t
											   nil "tor" selected-text
											   transform)) 
						   (setq result
								 (buffer-string))) 
	  (error "refac executable not found")) 
	(if (zerop exit-status) result
	  (error "refac returned a non-zero exit status: %d. Error: %s"
			 exit-status result))))

;; (global-set-key (kbd "C-c r") 'refac)

;; (defun refac (beg end transform)
;;   "Perform the refac transform in place, inserting merge markers inline."
;;   (interactive "r\nMTransform: ")
;;   (let* (
;; 		 (buffer (current-buffer))
;; 		 (original-text (buffer-substring-no-properties beg end))

;; 		 (_ (goto-char end))
;; 		 (_ (insert "\n=======\n"))

;; 		 ;; add an overlay here to track the future insertion point
;; 		 (overlay (make-overlay (point) (point) buffer t nil))

;; 		 (_ (insert ">>>>>>> TRANSFORMED\n"))
;; 		 (_ (goto-char beg))
;; 		 (_ (insert "<<<<<<< ORIGINAL\n"))

;; 		 (_ (smerge-mode 1))

;; 		 (new-text (refac-call-executable original-text transform)))

;; 	(goto-char (overlay-start overlay))
;; 	(insert new-text)
;; 	(delete-overlay overlay)))


;; (defun refac (beg end transform)
;;   "Call `refac-call-executable' on the contents of selected region using TRANSFORM.
;; `refac-call-executable` will return the transformed region.
;; Create an overlay in place with changes highlighted.
;; Prompt user to accept or deny the changes.
;; When accepted, write them back to the buffer, otherwise revert to original."
;;   (interactive "r\nMTransform: ")
;;   ;; Save original text
;;   (let* ((original-text (buffer-substring-no-properties beg end))
;;          (new-text (refac-call-executable original-text transform))
;;          (overlay (make-overlay beg end (current-buffer) t nil)))
;;     ;; Highlight proposed changes
;;     (overlay-put overlay 'face 'highlight)

;;     ;; Temporarily replace the region with new text
;;     (delete-region beg end)
;;     (goto-char beg)
;;     (insert new-text)

;;     ;; Prompt user
;;     (message "Proposed changes. Press (a)ccept or (b)deny.")
;;     (let ((response (read-char-exclusive)))
;;       (cond
;;        ((eq response ?a)
;;         ;; Accept changes
;;         (overlay-put overlay 'face nil)
;;         (delete-overlay overlay)
;;         (message "Changes accepted."))
;;        (t
;;         ;; Deny changes: revert
;;         (delete-region beg (+ beg (length new-text)))
;;         (goto-char beg)
;;         (insert original-text)
;;         (delete-overlay overlay)
;;         (message "Changes denied."))))))

;; (defun refac (beg end transform)
;;   "Call `refac-call-executable' on region [BEG..END] using TRANSFORM.
;; Then run `ediff-region-internal' to compare the region in the current buffer
;; to the transformed text in a temporary buffer, allowing you to merge changes."
;;   (interactive "r\nMTransform: ")
;;   (let ((original (buffer-substring-no-properties beg end))
;;         (temp-buf (generate-new-buffer "*my-refac-transformed*")))
;;     (unwind-protect
;;         (let ((transformed (refac-call-executable original transform)))
;;           (with-current-buffer temp-buf (insert transformed))
;;           (let ((tbeg (with-current-buffer temp-buf (point-min)))
;;                 (tend (with-current-buffer temp-buf (point-max))))
;;             (ediff-region-internal (current-buffer) beg end
;;                                    temp-buf tbeg tend
;;                                    nil 'wordwise)))
;;       (add-hook 'ediff-quit-hook
;;                 (lambda ()
;;                   (when (buffer-live-p temp-buf)
;;                     (kill-buffer temp-buf)))
;;                 t t))))


;; (defun refac (beg end transform)
;;   "Call `refac-call-executable' on region [BEG..END] using TRANSFORM.
;; Then run `ediff-regions-linewise' to compare the region in the current buffer
;; to the transformed text in a temporary buffer, allowing you to merge changes."
;;   (interactive "r\nMTransform: ")
;;   (let ((original (buffer-substring-no-properties beg end))
;;         (temp-buf (generate-new-buffer "*my-refac-transformed*")))
;;     (unwind-protect
;;         (progn
;;           ;; 1) Put transformed text in the temp buffer
;;           (with-current-buffer temp-buf
;;             (insert (refac-call-executable original transform)))

;;           ;; 2) Set the region in the original buffer
;;           (save-excursion
;;             (push-mark beg)
;;             (goto-char end)
;;             (activate-mark))

;;           ;; 3) Set the region in the transformed buffer
;;           (with-current-buffer temp-buf
;;             (push-mark (point-min))
;;             (goto-char (point-max))
;;             (activate-mark))

;;           ;; 4) Compare the two regions linewise
;;           (ediff-regions-linewise (current-buffer) temp-buf))

;;       ;; Clean up: kill the temp buffer when Ediff quits.
;;       (add-hook 'ediff-quit-hook
;;                 (lambda ()
;;                   (when (buffer-live-p temp-buf)
;;                     (kill-buffer temp-buf)))
;;                 ;; Append instead of prepend so we don't disrupt other ediff-quit-hooks
;;                 t t))))



;; (defun refac (beg end)
;;   "Call `refac-call-executable' on the region from BEG to END,
;; put both original and transformed text in temp buffers, then Ediff them."
;;   (interactive "r")
;;   (let* ((original (buffer-substring-no-properties beg end))
;; 		 (instruction (read-string "Transform: "))
;;          (transformed (refac-call-executable original instruction))
;;          (buf-A (get-buffer-create "*my-diff-A*"))
;;          (buf-B (get-buffer-create "*my-diff-B*")))
;;     ;; Prepare the two buffers
;;     (with-current-buffer buf-A
;;       (erase-buffer)
;;       (insert original)
;;       (set-buffer-modified-p nil))
;;     (with-current-buffer buf-B
;;       (erase-buffer)
;;       (insert transformed)
;;       (set-buffer-modified-p nil))
;;     ;; Now show Ediff
;;     (ediff-buffers buf-A buf-B)))


;; (defun partial-merge-refactor-region (start end)
;;   "Refactor only part of the current buffer, then partially merge changes via `diff-mode`.
;; After selecting a region, run this command.  It will:
;; 1) Prompt for transformation instructions.
;; 2) Create a temp file containing the 'after' version (with your region transformed).
;; 3) Diff that temp file against your on-disk file.
;; 4) Open the patch in a `diff-mode` buffer, letting you apply hunks selectively.

;; Keybindings in the patch buffer:
;;   n      Move to next hunk
;;   p      Move to previous hunk
;;   C-c C-a  Apply the current hunk
;;   C-c C-r  Revert the current hunk (undo it)
;;   q      Quit the patch buffer"
;;   (interactive "r")
;;   ;; Ensure the buffer is saved before we diff against disk.
;;   (when (buffer-modified-p)
;;     (when (yes-or-no-p "Buffer has unsaved changes. Save before generating diff? ")
;;       (save-buffer)))

;;   (let* ((orig-file (buffer-file-name (current-buffer)))
;;          (full-orig (buffer-substring-no-properties (point-min) (point-max)))
;;          (region-text (buffer-substring-no-properties start end))
;;          (instruction (read-string "Enter transformation instruction: "))
;;          ;; Transform only the region
;;          (new-region-text (refac-call-executable region-text instruction))
;;          ;; Construct the would-be new entire buffer
;;          (new-full
;;           (concat (substring full-orig 0 start)
;;                   new-region-text
;;                   (substring full-orig end)))
;;          ;; Temporary file to store the new version
;;          (tmp-file (make-temp-file "refac-" nil ".tmp"))
;;          ;; We'll store the diff buffer here
;;          diff-buf)

;;     ;; Write the new version to the temp file
;;     (with-temp-file tmp-file
;;       (insert new-full))

;;     ;; Now generate the diff in the background (synchronously in this case).
;;     ;; `diff-no-select` returns a buffer in `diff-mode` that isn't displayed yet.
;;     (setq diff-buf (diff-no-select orig-file tmp-file nil 'no-async))

;;     ;; Show the diff buffer to the user
;;     (switch-to-buffer diff-buf)
;;     (diff-mode)

;;     (message "Use n/p to navigate hunks, C-c C-a to apply, C-c C-r to revert, then q to quit.")))


;; (defun refac (start end)
;;   "Run refac on region, and compare original vs. transformed text with Ediff.
;; When you finish Ediff, the merged buffer (if any) will be placed in the kill ring,
;; so you can paste it back into your original buffer if desired."
;;   (interactive "r")
;;   (let* ((orig-text (buffer-substring-no-properties start end))
;;          (instruction (read-string "Enter transformation instruction: "))
;;          (new-text (refac-call-executable orig-text instruction)))
;;     (my-ediff-merge-region start end new-text)
;;     ;; Place merged text in kill ring after Ediff is closed.
;;     (add-hook 'ediff-quit-hook
;;               (lambda ()
;;                 (let ((merged-buf (get-buffer "*Ediff Merge*")))
;;                   (when (buffer-live-p merged-buf)
;;                     (with-current-buffer merged-buf
;;                       (kill-new (buffer-string))
;;                       (message "Ediff merged output copied to kill ring. Paste where you want it.")))))
;;               nil 'local)))


;; (defun my-ediff-merge-region (start end new-value)
;;   "Interactively merge changes from NEW-VALUE into the region between START and END."
;;   (interactive "r\nsNew value for region: ")
;;   (let* ((original-buffer (current-buffer))
;;          (original-content (buffer-substring-no-properties start end))
;;          (temp-buffer (generate-new-buffer "*ediff-region-temp*")))
;;     ;; Insert the new value into the temporary buffer
;;     (with-current-buffer temp-buffer
;;       (insert new-value))
;;     ;; Create an indirect buffer so we can narrow to the region
;;     (let ((region-buffer (make-indirect-buffer original-buffer "*ediff-region-orig*" t)))
;;       (with-current-buffer region-buffer
;;         (narrow-to-region start end))
;;       ;; Start ediff to merge the two
;;       (ediff-buffers region-buffer temp-buffer
;;                      (lambda ()
;;                        ;; Cleanup buffers after merging
;;                        (kill-buffer temp-buffer)
;;                        (kill-buffer region-buffer))))))

;; lookup snippent in explainshell.com
(defun explainshell (start end) 
  (interactive "r") 
  (let ((selected-text (buffer-substring-no-properties start end))) 
	(browse-url
	 (concat "https://explainshell.com/explain?cmd="
			 (url-hexify-string selected-text)))))
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
