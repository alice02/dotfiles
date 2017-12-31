;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; Caskを読み込む
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; 環境を日本語、UTF-8にする
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; テーマの設定
(load-theme 'monokai t)
  (setq ;; foreground and background
        monokai-foreground     "#ABB2BF"
        monokai-background     "#282C34"
        ;; highlights and comments
        monokai-comments       "#F8F8F0"
        monokai-emphasis       "#282C34"
        monokai-highlight      "#FFB269"
        monokai-highlight-alt  "#66D9EF"
        monokai-highlight-line "#1B1D1E"
        monokai-line-number    "#F8F8F0"
        ;; colours
        monokai-blue           "#61AFEF"
        monokai-cyan           "#56B6C2"
        monokai-green          "#98C379"
        monokai-gray           "#3E4451"
        monokai-violet         "#C678DD"
        monokai-red            "#E06C75"
        monokai-orange         "#D19A66"
        monokai-yellow         "#E5C07B")

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
(global-linum-mode t)

;; カーソル行をハイライトする
(global-hl-line-mode t)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; ペーストするときにインデントしないようにする
(electric-indent-mode 0)

;; C-hをbackspaceに割り当てる
(global-set-key "\C-h" 'delete-backward-char)

;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;; 行番号を左に表示
(global-linum-mode t)
(setq linum-format "%4d ")

;; ;; バックアップファイルの保存先を変更
;; (setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
;;  ;; 番号付けによる複数保存
;;  (setq version-control     t)  ;; 実行の有無
;;  (setq kept-new-versions   5)  ;; 最新の保持数
;;  (setq kept-old-versions   1)  ;; 最古の保持数
;;  (setq delete-old-versions t)  ;; 範囲外を削除
;; バックアップ保存先の変更
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
;; Company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )))

(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))

(define-key company-active-map [tab] 'company-complete-common2)
(define-key company-active-map [backtab] 'company-select-previous) ; おまけ

;; Golang関係
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(require 'go-mode)
(require 'company-go)
;; 諸々の有効化、設定
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda ()
                          (setq gofmt-command "goimports")
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (set (make-local-variable 'compile-command)
                               "go build -v && go test -v && go vet")
                          (local-set-key (kbd "M-.") 'godef-jump))
                          (go-eldoc-setup)
                          )

;; whitespace
(require 'whitespace)
(setq whitespace-style
      '(
        face ; faceで可視化
        trailing ; 行末
;        tabs ; タブ
        spaces ; スペース
        space-mark ; 表示のマッピング
        tab-mark
        ))
(setq whitespace-display-mappings
      '(
        (space-mark ?\u3000 [?\u2423])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
        ))
(setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-attribute 'whitespace-trailing nil
                    :foreground "RoyalBlue4"
                    :background "RoyalBlue4"
                    :underline nil)
(set-face-attribute 'whitespace-tab nil
                    :foreground "yellow4"
                    :background "yellow4"
                    :underline nil)
(set-face-attribute 'whitespace-space nil
                    :foreground "gray40"
                    :background "gray20"
                    :underline nil)
(global-whitespace-mode t)

;; Jedi
(require 'jedi-core)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-to-list 'company-backends 'company-jedi)

;; Javascriptのインデント幅を2にする
(setq js-indent-level 2)

;; Javascriptでタブインデントしない
(add-hook 'js-mode-hook
          (function
           (lambda ()
             (setq indent-tabs-mode nil)
             )))

(require 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=200"))
(setq flycheck-flake8-maximum-line-length 200)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
(setq flymake-python-pyflakes-extra-arguments '("--ignore=E501"))

(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "$HOME/.anyenv/envs/pyenv/shims/pyflakes"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

; show message on mini-buffer
(defun flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)

(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)

; show message on mini-buffer
(defun flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)

;;powerlineの設定
(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#FF0066"
                    :box nil)

(set-face-attribute 'powerline-active1 nil
                    :foreground "#fff"
                    :background "#FF6699"
                    :inherit 'mode-line)

(set-face-attribute 'powerline-active2 nil
                    :foreground "#000"
                    :background "#ffaeb9"
                    :inherit 'mode-line)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; hide menu bar
(menu-bar-mode -1)

;; helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)
(define-key global-map (kbd "M-r")     'helm-resume)
(define-key global-map (kbd "C-M-h")   'helm-apropos)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(define-key yaml-mode-map "\C-m" 'newline-and-indent)


;; tabbar
(require 'tabbar)
(tabbar-mode)
(tabbar-mwheel-mode nil)                  ;; マウスホイール無効
(setq tabbar-buffer-groups-function nil)  ;; グループ無効
(setq tabbar-use-images nil)              ;; 画像を使わない
;;----- キーに割り当てる
(global-set-key (kbd "<f10>") 'tabbar-forward-tab)
(global-set-key (kbd "<f9>") 'tabbar-backward-tab)

;;----- 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
;;----- タブのセパレーターの長さ
(setq tabbar-separator '(2.0))
;;----- 表示するバッファ
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
                     ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; js, jsx
;; .js, .jsx を web-mode で開く
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))

;; 拡張子 .js でもJSX編集モードに
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; インデント
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
          ))
;; 色
(custom-set-faces
 '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
 '(web-mode-comment-face           ((t (:foreground "#587F35"))))
 '(web-mode-server-comment-face    ((t (:foreground "#587F35"))))

 '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
 '(web-mode-comment-face           ((t (:foreground "#587F35"))))
 '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
 '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
 )

(setq company-dabbrev-downcase nil)

;; multiple cursors
(require 'multiple-cursors)
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-*")   'mc/mark-all-like-this)
(global-unset-key "\C-q")
(smartrep-define-key global-map "C-q"
  '(("C-p"      . 'mc/mark-previous-like-this)
    ("C-n"      . 'mc/mark-next-like-this)
    ("u" . mc/unmark-next-like-this)
    ("U" . mc/unmark-previous-like-this)
    ("s" . mc/skip-to-next-like-this)
    ("S" . mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)))

;; --------------------------------------------------
;; ruby-mode
;; http://shibayu36.hatenablog.com/entry/2013/03/18/192651
;; --------------------------------------------------
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)) ;; shebangがrubyの場合、ruby-modeを開く

;; ruby-modeのインデントを改良する
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; --------------------------------------------------
;; ruby-end
;; endや括弧などを自動挿入する
;; http://blog.livedoor.jp/ooboofo3/archives/53748087.html
;; --------------------------------------------------
(require 'ruby-end)
(add-hook 'ruby-mode-hook
  '(lambda ()
    (abbrev-mode 1)
    (electric-pair-mode t)
    (electric-indent-mode t)
    (electric-layout-mode t)))

;; --------------------------------------------------
;; ruby-block
;; endにカーソルを合わせると、そのendに対応する行をハイライトする
;; --------------------------------------------------
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
