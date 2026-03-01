;;; config-os.el --- OS detection and platform-specific settings  -*- lexical-binding: t; -*-
;;; Commentary:
;; Sets my/os to 'linux, 'macos, or 'windows.
;; Adjusts font paths, shell, modifier keys, and paths per platform.
;; Load this FIRST so everything else can use my/os.

;;; Code:

;; ─── Detect OS ───────────────────────────────────────────────────────────────
(defconst my/os
  (cond ((eq system-type 'gnu/linux)  'linux)
        ((eq system-type 'darwin)     'macos)
        ((memq system-type '(windows-nt ms-dos cygwin)) 'windows)
        (t 'linux))
  "Detected OS: 'linux, 'macos, or 'windows.")

(defmacro my/on-linux   (&rest body) `(when (eq my/os 'linux)   ,@body))
(defmacro my/on-macos   (&rest body) `(when (eq my/os 'macos)   ,@body))
(defmacro my/on-windows (&rest body) `(when (eq my/os 'windows) ,@body))

;; ─── Linux ───────────────────────────────────────────────────────────────────
(my/on-linux
 ;; Default shell for M-x shell
 (setq explicit-shell-file-name (or (executable-find "zsh")
                                    (executable-find "bash")
                                    "/bin/sh"))
 ;; Use XDG dirs
 (setq xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                            (expand-file-name "~/.config"))))

;; ─── macOS ───────────────────────────────────────────────────────────────────
(my/on-macos
 ;; Remap modifier keys to feel natural on Mac keyboard
 (setq mac-command-modifier 'meta        ; Cmd  = M-
       mac-option-modifier  'super       ; Opt  = s-
       mac-control-modifier 'control)    ; Ctrl = C-
 ;; Default shell
 (setq explicit-shell-file-name (or (executable-find "zsh") "/bin/zsh"))
 ;; Homebrew PATH
 (let ((homebrew-paths '("/opt/homebrew/bin"       ; Apple Silicon
                         "/usr/local/bin"           ; Intel
                         "/opt/homebrew/sbin")))
   (dolist (p homebrew-paths)
     (when (file-directory-p p)
       (setenv "PATH" (concat p ":" (getenv "PATH")))
       (add-to-list 'exec-path p))))
 ;; Fix ls flags (macOS ls doesn't support --group-directories-first)
 (setq dired-listing-switches "-lAh")
 (setq insert-directory-program "gls")  ; brew install coreutils
 ;; Fullscreen on macOS
 (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; ─── Windows ─────────────────────────────────────────────────────────────────
(my/on-windows
 ;; Prefer PowerShell Core if available, fall back to cmd
 (setq explicit-shell-file-name
       (or (executable-find "pwsh")
           (executable-find "powershell")
           "cmd.exe"))
 ;; Windows line endings
 (set-default-coding-systems 'utf-8)
 (setq-default buffer-file-coding-system 'utf-8-unix)
 ;; Common Windows tool paths
 (let ((win-paths '("C:/Program Files/Git/usr/bin"
                    "C:/Windows/System32")))
   (dolist (p win-paths)
     (when (file-directory-p p)
       (add-to-list 'exec-path p))))
 ;; Disable ls --dired on Windows
 (setq ls-lisp-use-insert-directory-program nil)
 (require 'ls-lisp))



(provide 'config-os)
;;; config-os.el ends here
