                                        ; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ load-path

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp")

;; ------------------------------------------------------------------------
;; @ general

;; common lisp
(require 'cl)

;; 文字コード
(set-language-environment "Japanese")
(let ((ws window-system))
  (cond ((eq ws 'w32)
	 (prefer-coding-system 'utf-8-unix)
	 (set-default-coding-systems 'utf-8-unix)
	 (setq file-name-coding-system 'sjis)
	 (setq locale-coding-system 'utf-8))
	((eq ws 'ns)
	 (require 'ucs-normalize)
	 (prefer-coding-system 'utf-8-hfs)
	 (setq file-name-coding-system 'utf-8-hfs)
	 (setq locale-coding-system 'utf-8-hfs))))

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
	 (set-face-attribute 'default nil
			     :family "Meiryo"  ;; 英数
			     :height 100)
	 (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo")))  ;; 日本語
	((eq ws 'ns)
	 (set-face-attribute 'default nil
			     :family "Ricty"  ;; 英数
			     :height 140)
	 (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))))  ;; 日本語

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバーを非表示
;;(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
		    :foreground "#fffff0"
		    :height 0.9)

;; 行番号フォーマット
(setq linum-format "%4d")

;; 括弧の範囲内を強調表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#ffffe0")

;; 行末の空白を強調表示
;;(setq-default show-trailing-whitespace t)
;;(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables '(tab-width 4))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)

;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; ------------------------------------------------------------------------
;; @ modeline

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))
  
  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))

;; ------------------------------------------------------------------------
;; @ auto-install.el

;; パッケージのインストールを自動化
;; http://www.emacswiki.org/emacs/auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))


;; ------------------------------------------------------------------------
;; @ menu-tree.el

;; メニューバー日本語化
;; http://www11.atwiki.jp/s-irie/pages/13.html
(if (and (= emacs-major-version 22)
         (eq window-system 'x))
    (setq menu-tree-coding-system 'utf-8))
;;(require 'menu-tree nil t)


;; ------------------------------------------------------------------------
;; @ tabbar.el

;; タブ化
;; http://www.emacswiki.org/emacs/tabbar.el
;;(require 'cl)
(require 'tabbar nil t)

;; scratch buffer以外をまとめてタブに表示する
(setq tabbar-buffer-groups-function
      (lambda (b) (list "All Buffers")))
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda(buffer)
           (unless (string-match (buffer-name buffer)
                                 "\\(*scratch*\\|*Apropos*\\|*shell*\\|*eshell*\\|*Customize*\\)")
             (find (aref (buffer-name buffer) 0) " *"))
           )
         (buffer-list))))

;; tabbarを有効にする
(tabbar-mode 1)

;; ボタンをシンプルにする
(setq tabbar-home-button-enabled "")
(setq tabbar-scroll-right-button-enabled "")
(setq tabbar-scroll-left-button-enabled "")
(setq tabbar-scroll-right-button-disabled "")
(setq tabbar-scroll-left-button-disabled "")

;; Ctrl-Tab, Ctrl-Shift-Tab でタブを切り替える
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
(global-set-key [(control tab)] 'shk-tabbar-next)
(global-set-key [(control shift tab)] 'shk-tabbar-prev)

;; GUIで直接ファイルを開いた場合フレームを作成しない
(add-hook 'before-make-frame-hook
          (lambda ()
            (when (eq tabbar-mode t)
              (switch-to-buffer (buffer-name))
                            (delete-this-frame))))
;; ------------------------------------------------------------------------
;; @ auto-async-byte-compile.el

;; 自動コンパイル
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(when (require 'auto-async-byte-compile nil t)
  ;; 自動コンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;; ------------------------------------------------------------------------
;; @ recentf-ext.el
;; 自動保存
(when (require 'recentf-ext nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

;; 起動画面削除
(setq inhibit-startup-message t)

;; 起動画面で recentf を開く
(add-hook 'after-init-hook (lambda()
                             (recentf-open-files)
                             ))

;; キーバインド
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; ------------------------------------------------------------------------
;; @ auto-complete.el
(require 'auto-complete)
(require 'auto-complete-config)
;; グローバルでauto-completeを利用
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "M-n") 'ac-next)      ; M-nで次候補選択
(define-key ac-completing-map (kbd "M-p") 'ac-previous)  ; M-pで前候補選択
(setq ac-dwim t)  ; 空気読んでほしい
;; 情報源として
;; * ac-source-filename
;; * ac-source-words-in-same-mode-buffers
;; を利用
(setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
;; また、Emacs Lispモードではac-source-symbolsを追加で利用
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
;; 以下、自動で補完する人用
;(setq ac-auto-start 3)
;; 以下、手動で補完する人用
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")  ; TABで補完開始(トリガーキー)
;; or
    (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)  ; M-TABで補完開始

;; ------------------------------------------------------------------------
;; @ rainbow-delimiters.el
(require 'rainbow-delimiters)

;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(global-rainbow-delimiters-mode)

(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
      (cl-callf color-saturate-name (face-foreground face) 30)))

;; ------------------------------------------------------------------------
;; @ drill-instructor.el(鬼軍曹)
;;(require 'drill-instructor)
;;(setq drill-instructor-global t)

;; ------------------------------------------------------------------------
;; (Cのインデントにあわせる)
;; http://w.livedoor.jp/whiteflare503/d/Emacs%20%A5%A4%A5%F3%A5%C7%A5%F3%A5%C8
;;共通設定(?)
(setq-default c-basic-offset 4     ;;基本インデント量4
              tab-width 4          ;;タブ幅4
              indent-tabs-mode nil)  ;;インデントをタブでするかスペースでするか

;; C++ style
(defun add-c++-mode-conf ()
  (c-set-style "stroustrup")  ;;スタイルはストラウストラップ
  (show-paren-mode t))        ;;カッコを強調表示する
(add-hook 'c++-mode-hook 'add-c++-mode-conf)

;; C style
(defun add-c-mode-common-conf ()
  (c-set-style "stroustrup")                  ;;スタイルはストラウストラップ
  (show-paren-mode t)                         ;;カッコを強調表示する
  )
(add-hook 'c-mode-common-hook 'add-c-mode-common-conf)

;; ------------------------------------------------------------------------
;; (gdbの設定)

(require 'gud)

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(add-hook
 'gdb-mode-hook
 '(lambda ()
    (gud-tooltip-mode t)
    (gud-def gud-break-main "break main" nil "Set breakpoint at main.")
    ))
(setq gud-tooltip-echo-area nil)

(define-key gud-minor-mode-map (kbd "<f1>") 'gud-print)
(define-key gud-minor-mode-map (kbd "<S-f1>") 'gud-watch)
(define-key gud-minor-mode-map (kbd "<f2>") 'gud-refresh)
(define-key gud-minor-mode-map (kbd "<f5>") 'gud-cont)
(define-key gud-minor-mode-map (kbd "<S-f5>") 'gud-kill)
(define-key gud-minor-mode-map (kbd "<f6>") 'gud-until)
(define-key gud-minor-mode-map (kbd "<f9>") 'gdb-set-clear-breakpoint)
(define-key gud-minor-mode-map (kbd "<S-f9>") 'gud-break-main)
(define-key gud-minor-mode-map (kbd "<f10>") 'gud-next)
(define-key gud-minor-mode-map (kbd "<f11>") 'gud-step)
(define-key gud-minor-mode-map (kbd "<C-f10>") 'gud-until)
(define-key gud-minor-mode-map (kbd "<C-f11>") 'gud-finish)
(define-key gud-minor-mode-map (kbd "<S-f11>") 'gud-finish)

(defun gdb-set-clear-breakpoint ()
  (interactive)
  (if (or (buffer-file-name) (eq major-mode 'gdb-assembler-mode))
      (if (or
           (let ((start (- (line-beginning-position) 1))
                 (end (+ (line-end-position) 1)))
             (catch 'breakpoint
               (dolist (overlay (overlays-in start end))
                 (if (overlay-get overlay 'put-break)
                     (throw 'breakpoint t)))))
           (eq (car (fringe-bitmaps-at-pos)) 'breakpoint))
          (gud-remove nil)
        (gud-break nil))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer)))

;; ------------------------------------------------------------------------
;; C++ style
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "stroustrup")
             (setq indent-tabs-mode nil)     ; インデントは空白文字で行う（TABコードを空白に変換）
             (c-set-offset 'innamespace 0)   ; namespace {}の中はインデントしない
             (c-set-offset 'arglist-close 0) ; 関数の引数リストの閉じ括弧はインデントしない
             ))
;; 全自動インデントを有効
(setq c-auto-newline t)
