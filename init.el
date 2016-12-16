(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq ring-bell-function 'ignore)

(setq create-lockfiles nil)
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

(el-get-bundle tarao/with-eval-after-load-feature-el)

(el-get-bundle twilight-anti-bright-theme
  (require 'twilight-anti-bright-theme))

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;;@ key bind
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c f") 'windmove-right)

(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")
(global-set-key (kbd "C-c l") 'goto-line)

(setq-default indent-tabs-mode nil)

;;paredit
(el-get-bundle paredit
  (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'enable-paredit-mode)
  (add-hook 'inferior-scheme-mode-hook  'enable-paredit-mode)
  (with-eval-after-load-feature 'paredit
    (define-key paredit-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)))

(el-get-bundle yasnippet
  (require 'yasnippet)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"                 ;; personal snippets
	  "~/.emacs.d/el-get/yasnippet/snippets"         ;; the default collection
	  ))
  (define-key yas-keymap (kbd "<tab>") nil)
  (yas-global-mode 1))

(el-get-bundle company
  (require 'company)
  (global-company-mode +1)
  (setq company-dabbrev-downcase nil)

  (set-face-attribute 'company-tooltip nil
		      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
		      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
		      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
		      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
		      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
		      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
		      :background "gray40")

  (global-set-key (kbd "C-M-i") 'company-complete)

  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-search-map (kbd "TAB") 'company-select-next)

  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  (define-key company-active-map (kbd "C-i") 'company-complete-selection)

  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

  (setq company-idle-delay 0)
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
	(backward-char 1)
	(if (looking-at "\\.") t
	  (backward-char 1)
	  (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
	(minibuffer-complete)
      (if (or (not yas-minor-mode)
	      (null (do-yas-expand)))
	  (if (check-expansion)
	      (company-complete-common)
	    (indent-for-tab-command)))))

  (setq company-selection-wrap-around t)

  (global-set-key (kbd "TAB") 'tab-indent-or-complete)
  (setq company-backends
        '(company-bbdb
          company-nxml
          company-css
          company-eclim
          company-semantic
                                        ;        company-clang
          company-xcode
          company-cmake
          company-capf
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse company-files company-dabbrev)
        ))

(setq company-dabbrev-downcase nil)

(el-get-bundle company-statistics
  (require 'company-statistics)
  (add-hook 'after-init-hook 'company-statistics-mode)
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance)))

;; company-quickhelp and its dependency
(el-get-bundle pos-tip)

(el-get-bundle company-quickhelp
  (require 'pos-tip)
  (setq pos-tip-foreground-color "white")
  (setq pos-tip-background-color "steelblue")
  (with-eval-after-load-feature 'company
    (company-quickhelp-mode 1))
  (with-eval-after-load-feature 'company-quickhelp
    (setq company-quickhelp-delay 0.5)
    (setq company-quickhelp-max-lines 10)))

(el-get-bundle rainbow-delimiters
  (require 'rainbow-delimiters)
  (require 'color)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))


(el-get-bundle anzu
  (require 'anzu)
  (global-anzu-mode +1)
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " => "))
  (define-key isearch-mode-map [remap isearch-query-replace] 'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
  (define-key global-map (kbd "C-t") 'anzu-query-replace-regexp))


;; ido
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(el-get-bundle smex
  (require 'smex)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(el-get-bundle ido-ubiquitous
  (require 'ido-ubiquitous)
  (ido-ubiquitous-mode 1))
(el-get-bundle ido-vertical-mode
  (require 'ido-vertical-mode)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))


(el-get-bundle glsl-mode
  (autoload 'glsl-mode "glsl-mode" nil t))

(el-get-bundle web-mode
  (autoload 'web-mode "web-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.html" . web-mode))
  (with-eval-after-load-feature 'web-mode
    (add-hook 'web-mode-hook 'electric-pair-mode)))

;; javascript-mode
(add-hook 'js-mode-hook 'electric-pair-mode)
(add-hook 'js-mode-hook 'subword-mode)

(el-get-bundle slime-company)
(el-get-bundle slime)

(require 'slime)
(setq slime-contribs '(slime-repl slime-fancy slime-banner slime-company))
(setf slime-lisp-implementations
      `((ccl64    ("wx86cl64"))
        (sbcl    ("sbcl" "--dynamic-space-size" "2000"))
        (roswell ("ros" "dynamic-space-size=2000" "-Q" "-l" "~/.sbclrc" "run"))))
(setf slime-default-lisp 'roswell)
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)


(el-get-bundle undo-tree
  (require 'undo-tree)
  (global-undo-tree-mode t)
  (global-set-key (kbd "M-/") 'undo-tree-redo))

(el-get-bundle yatex
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  (add-to-list 'auto-mode-alist '("\\.tex" . yatex-mode))
  (with-eval-after-load 'yatex-mode
    (setq-default tex-command "platex")
    (setq-default dviprint-command-format "dvipdfmx %s ")))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default c-basic-offset 4)
(setq-default intent-tabs-mode nil)
(setq-default tab-width 4)

(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (c-set-offset 'arglist-close 0)
             ))
(add-hook 'opencl-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (c-set-offset 'arglist-close 0)))

(autoload 'c++ "c++-mode" nil t)
(add-hook 'c++-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)
(setq company-clang-executable "clang-3.5")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(el-get-bundle opencl-mode
  (autoload 'opencl-mode "opencl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode)))

(el-get-bundle markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)
  (with-eval-after-load-feature 'markdown-mode
    (autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
    (autoload 'gfm-mode "gfm-mode" "Major mode for editing GitHub Flavored Markdown files" t)
    (setq markdown-command "pandoc -f markdown_github -t html5 --mathjax"))))


(el-get-bundle ddskk
  (require 'skk)
  (require 'skk-study)
  (setq default-input-method "japanese-skk")
  (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
  (setq skk-henkan-strict-okuri-precedence t)
  (setq skk-check-okurigana-on-touroku t)
  (setq skk-show-inline nil)
  (setq skk-show-tooltip nil)
  (setq skk-show-candidates-always-pop-to-buffer t)
  (setq skk-show-annotation t)
  (setq skk-annotation-delay 0.5)
  (setq skk-dcomp-activate t)
  (setq skk-henkan-show-candidates-rows 2)
  (setq skk-dcomp-multiple-activate t)
  (setq skk-dcomp-multiple-rows 10)
  (setq skk-show-japanese-menu nil)
  (setq skk-japanese-message-and-error nil)
  (setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
  (setq skk-tut-file "~/.emacs.d/el-get/ddskk/etc/SKK.tut")
  (setq skk-kuten-touten-alist
        (cons '(my-en ". " . ", ")
              skk-kuten-touten-alist))
  (setq-default skk-kutouten-type 'my-en))
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))

;; ------------------
;; package

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (require 'magit nil 'noerror)
  (package-install 'dash)
  (package-install 'with-editor)
  (package-install 'git-commit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(unless (require 'cider nil 'noerror)
  (package-install 'cider))
(autoload 'cider "cider" nil t)
(add-hook 'cider-mode-hook  'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook  'enable-paredit-mode)
(setq nrepl-log-messages t
      cider-repl-display-in-current-window t
      cider-repl-use-clojure-font-lock t
      cider-prompt-save-file-on-load 'always-save
      cider-font-lock-dynamically '(macro core function var)
      cider-overlays-use-font-lock t)
(cider-repl-toggle-pretty-printing)
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(unless (require 'clj-refactor nil 'noerror)
  (package-install 'clj-refactor))
(add-hook 'cider-mode-hook 'clj-refactor-mode)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
;;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;; https://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; color http://www.clear-code.com/blog/2012/4/3.html
(defun diff-mode-setup-faces ()
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "dark green")
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "dark red")
  (set-face-attribute 'diff-refine-change nil
                      :foreground nil :background nil
                      :weight 'bold :inverse-video t))

(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)

(setq magit-diff-refine-hunk 'all)


