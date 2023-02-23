;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; byte compile command:emacs -Q --batch -f batch-byte-compile init.el

;;; Code:

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf twilight-anti-bright-theme
  :doc "A soothing Emacs 24 light-on-dark theme"
  :tag "themes"
  :url "https://github.com/jimeh/twilight-anti-bright-theme.el"
  :added "2022-02-11"
  :ensure t
  :config
  (load-theme 'twilight-anti-bright t))

(leaf leaf
  :custom ((ring-bell-function . 'ignore)
           (create-lockfiles . nil)
           (indent-tabs-mode . nil))
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  (setq backup-directory-alist
        (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
              backup-directory-alist))
  (setq auto-save-file-name-transforms
        `((".*", (expand-file-name "~/.emacs.d/backup/") t)))
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (global-set-key (kbd "C-c <left>") 'windmove-left)
  (global-set-key (kbd "C-c <down>") 'windmove-down)
  (global-set-key (kbd "C-c <up>") 'windmove-up)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c b") 'windmove-left)
  (global-set-key (kbd "C-c n") 'windmove-down)
  (global-set-key (kbd "C-c p") 'windmove-up)
  (global-set-key (kbd "C-c f") 'windmove-right)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
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
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf linum
  :ensure t
  :custom
  (linum-format . "%4d ")
  :config
  (global-linum-mode 1))

(leaf paredit
  :ensure t
  :hook (emacs-lisp-mode-hook
         lisp-interaction-mode-hook
         eval-expression-minibuffer-setup-hook
         lisp-mode-hook)
  :bind (("<C-backspace>" . paredit-backward-kill-word)))

(leaf ido
  :defvar ido-enable-flex-matching
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (leaf ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode 1))
  (leaf amx
    :ensure t
    :config
    (amx-mode 1))
  (leaf ido-yes-or-no
    :ensure t
    :config
    (ido-yes-or-no-mode 1))
  (leaf crm-custom
    :ensure t
    :config
    (crm-custom-mode 1))
  (leaf ido-vertical-mode
    :doc "Makes ido-mode display vertically"
    :req "emacs-24.4"
    :tag "convenience" "emacs>=24.4"
    :url "https://github.com/creichert/ido-vertical-mode.el"
    :added "2022-02-12"
    :emacs>= 24.4
    :ensure t
    :defvar ido-vertical-define-keys
    :config
    (ido-vertical-mode 1)
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "dash-20210826" "git-commit-20211004" "magit-section-20211004" "transient-20210920" "with-editor-20211001"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2022-02-12"
  :emacs>= 25.1
  :ensure t
  :bind (("C-x g" . magit-status))
  :after ido
  :defvar magit-completing-read-function
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(leaf anzu
  :doc "Show number of matches in mode-line while searching"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://github.com/emacsorphanage/anzu"
  :added "2022-02-12"
  :emacs>= 25.1
  :ensure t
  :custom
  ((anzu-mode-lighter . "")
   (anzu-deactivate-region . t)
   (anzu-search-threshold . 1000)
   (anzu-replace-threshold . 50)
   (anzu-replace-to-string-separator . " => "))
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map [remap isearch-query-replace] 'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] 'anzu-isearch-query-replace-regexp)
  (define-key global-map (kbd "C-t") 'anzu-query-replace-regexp))

(leaf skk
  :ensure ddskk
  :custom ((default-input-method . "japanese-skk"))
  :defvar skk-large-jisyo skk-kuten-touten-alist
  :config
  (setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
  (setq skk-kuten-touten-alist '(
	(jp . ("．" . "，"))
	(en . (". " . ", "))
        ))
  (setq-default skk-kutouten-type 'en)
  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-25.1"
  :tag "matching" "convenience" "abbrev" "emacs>=25.1"
  :url "http://company-mode.github.io/"
  :added "2022-02-12"
  :emacs>= 25.1
  :ensure t
  :config
  (leaf company-posframe
      :doc "Use a posframe as company candidate menu"
      :ensure t
      :after company
      :custom ((company-posframe-mode . t))))

(leaf yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/snippets"))
  :config
  (leaf yasnippet-snippets
    :doc "Collection of yasnippet snippets"
    :req "yasnippet-0.8.0"
    :tag "snippets"
    :url "https://github.com/AndreaCrotti/yasnippet-snippets"
    :added "2022-02-12"
    :ensure t
    :after yasnippet))

(leaf company
  :ensure t
  :custom ((company-dabbrev-downcase . nil)
           (company-dabbrev-ignore-case . nil)
           (company-idle-delay . 0)
           (company-selection-wrap-around . t))
  :defvar company-backends company-active-map
  :defun company-mode/backend-with-yas
  :config
  (global-company-mode +1)
  (setq company-backends
        '(company-bbdb
          company-semantic
          company-cmake
          company-capf
          company-clang
          company-files
          (company-yasnippet company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse
          company-dabbrev))
  (define-key company-active-map [tab] 'company-select-next)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (leaf company-statistics
    :ensure t
    :hook (after-init-hook)
    :defvar company-transformers
    :config
    (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))))

(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :added "2022-02-12"
  :ensure t
  :hook (prog-mode-hook)
  :defvar rainbow-delimiters-max-face-count
  :defun color-saturate-name
  :config
  (require 'color)
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
       (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

(leaf glsl-mode
  :doc "major mode for Open GLSL shader files"
  :tag "vulkan" "spir-v" "gpu" "opengl" "languages"
  :url "https://github.com/jimhourihan/glsl-mode"
  :added "2022-02-12"
  :ensure t
  :defvar c-basic-offset
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

(leaf undo-tree
  :doc "Treat undo history as a tree"
  :tag "tree" "history" "redo" "undo" "files" "convenience"
  :url "http://www.dr-qubit.org/emacs.php"
  :added "2022-02-12"
  :ensure t
  :bind (("M-/" . undo-tree-redo))
  :config
  (global-undo-tree-mode t))

(leaf electric-pair-mode
  :hook (js-mode-hook c++-mode-hook ))

(leaf subword-mode
  :hook (js-mode-hook c++-mode-hook))

(leaf vue-mode
  :doc "Major mode for vue component based on mmm-mode"
  :req "mmm-mode-0.5.5" "vue-html-mode-0.2" "ssass-mode-0.2" "edit-indirect-0.1.4"
  :tag "languages"
  :added "2022-02-12"
  :ensure t
  :config
  (add-hook 'vue-mode-hook #'lsp-deferred)
  )

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :url "https://web-mode.org"
  :added "2022-02-12"
  :emacs>= 23.1
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html" . web-mode)))

;; (leaf tern
;;   :doc "Tern-powered JavaScript integration"
;;   :req "json-1.2" "cl-lib-0.5" "emacs-24"
;;   :tag "emacs>=24"
;;   :url "http://ternjs.net/"
;;   :added "2022-02-12"
;;   :emacs>= 24
;;   :ensure t
;;   :config
;;   (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;;   (add-to-list 'company-backends 'company-tern))

(leaf js2-mode
  :doc "Improved JavaScript editing mode"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "javascript" "languages" "emacs>=24.1"
  :url "https://github.com/mooz/js2-mode/"
  :added "2022-02-12"
  :emacs>= 24.1
  :ensure t
  :custom ((js2-mode-show-parse-errors . nil)
           (js2-mode-show-strict-warnings . nil)
           (js2-idle-timer-delay . 0.5)
           ;(company-tern-meta-as-single-line . t)
           ;(company-tern-property-marker . " <p>")
           )
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
                                        ;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  (add-hook 'js2-mode-hook #'lsp-deferred)
  )

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :added "2022-02-12"
  :emacs>= 24.3
  :ensure t
  :defun my/use-eslint-from-node-modules
  :defvar flycheck-disabled-checkers flycheck-gcc-language-standard
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; flycheck setting for eslint
  ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
  ;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html#configuring_emacs
  (setf flycheck-disabled-checkers
        '(javascript-jshint json-jsonlint))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11"))))

(leaf slime
  :doc "Superior Lisp Interaction Mode for Emacs"
  :req "cl-lib-0.5" "macrostep-0.9"
  :tag "slime" "lisp" "languages"
  :url "https://github.com/slime/slime"
  :added "2022-02-12"
  :ensure t
  :defvar slime-lisp-implementations slime-default-lisp slime-repl-mode-map paredit-backward-delete-key
  :config
  (setf slime-lisp-implementations
      `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
        (roswell ("ros" "-Q" "run"))))
  (setf slime-default-lisp 'roswell)
  (defun override-slime-repl-bindings-with-paredit ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
  (add-to-list 'auto-mode-alist '("\\.ros" . lisp-mode))
  (leaf slime-company
    :doc "slime completion backend for company mode"
    :req "emacs-24.4" "slime-2.13" "company-0.9.0"
    :tag "abbrev" "lisp" "convenience" "emacs>=24.4"
    :added "2022-02-12"
    :emacs>= 24.4
    :ensure t
    :config
    (setq slime-contribs '(slime-repl slime-fancy slime-banner slime-company))))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2022-11-20"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv
  :defvar lsp-keymap-prefix lsp-mode-map lsp-command-map
  :hook (js2-mode . lsp-deferred)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :added "2022-11-20"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode markdown-mode
    :commands lsp-ui-mode)
  (leaf lsp-treemacs
    :doc "LSP treemacs"
    :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.0" "treemacs-2.5" "lsp-mode-6.0"
    :tag "languages" "emacs>=26.1"
    :url "https://github.com/emacs-lsp/lsp-treemacs"
    :added "2022-11-20"
    :emacs>= 26.1
    :ensure t
    :after treemacs lsp-mode)
  )

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
