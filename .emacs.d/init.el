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
;;(load-theme 'deeper-blue' t)

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

;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる
;;(setq show-paren-style 'mixed)
;;(set-face-background 'show-paren-match-face "grey")
;;(set-face-foreground 'show-paren-match-face "black")

;; スペース、タブなどを可視化する
;;(global-whitespace-mode 1)

;; スクロールは１行ごとに
;;(setq scroll-conservatively 1)

;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;; 行番号を左に表示
(global-linum-mode t)
(setq linum-format "%4d ")

;; トラックパッド用のスクロール設定
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)

;; バックアップファイルの保存先を変更
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
 ;; 番号付けによる複数保存
 (setq version-control     t)  ;; 実行の有無
 (setq kept-new-versions   5)  ;; 最新の保持数
 (setq kept-old-versions   1)  ;; 最古の保持数
 (setq delete-old-versions t)  ;; 範囲外を削除

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
;; PATHを通しておく
;; go getで取得したものを使いたいため
;; パスを通す
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
;; 必要なパッケージのロード
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
;; (py-autopep8-enable-on-save)
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
      (list "$HOME/.pyenv/shims/flake8"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
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


;; (defun powerline-my-theme ()
;;   "Setup the my mode-line."
;;   (interactive)
;;   (setq powerline-current-separator 'utf-8)
;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (mode-line (if active 'mode-line 'mode-line-inactive))
;;                           (face1 (if active 'mode-line-1-fg 'mode-line-2-fg))
;;                           (face2 (if active 'mode-line-1-arrow 'mode-line-2-arrow))
;;                           (separator-left (intern (format "powerline-%s-%s"
;;                                                           (powerline-current-separator)
;;                                                           (car powerline-default-separator-dir))))
;;                           (lhs (list (powerline-raw " " face1)
;;                                      (powerline-major-mode face1)
;;                                      (powerline-raw " " face1)
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-buffer-id nil )
;;                                      (powerline-raw " [ ")
;;                                      (powerline-raw mode-line-mule-info nil)
;;                                      (powerline-raw "%*" nil)
;;                                      (powerline-raw " |")
;;                                      (powerline-process nil)
;;                                      (powerline-vc)
;;                                      (powerline-raw " ]")
;;                                      ))
;;                           (rhs (list (powerline-raw "%4l" 'l)
;;                                      (powerline-raw ":" 'l)
;;                                      (powerline-raw "%2c" 'l)
;;                                      (powerline-raw " | ")
;;                                      (powerline-raw "%6p" )
;;                                      (powerline-raw " ")
;;                                      )))
;; (concat (powerline-render lhs)
;; 	(powerline-fill nil (powerline-width rhs))
;; 	(powerline-render rhs)))))))
;; (defun make/set-face (face-name fg-color bg-color weight)
;;   (make-face face-name)
;;   (set-face-attribute face-name nil
;;                       :foreground fg-color :background bg-color :box nil :weight weight))
;; (make/set-face 'mode-line-1-fg "#282C34" "#EF8300" 'bold)
;; (make/set-face 'mode-line-2-fg "#AAAAAA" "#2F343D" 'bold)
;; (make/set-face 'mode-line-1-arrow  "#AAAAAA" "#3E4451" 'bold) ;;この二行に同じ色を指定
;; (make/set-face 'mode-line-2-arrow  "#AAAAAA" "#3E4451" 'bold) ;;この二行に同じ色を指定
;; (powerline-my-theme)

;; rainbow-delimiters-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; load theme
(load-theme 'hc-zenburn t)

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
