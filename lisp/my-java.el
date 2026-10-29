;;; my-java.el --- Java via JDTLS (opt-in: enable in init.el) -*- lexical-binding: t; -*-

(require 'my-settings)

(defvar my/jdtls-dir (expand-file-name "~/.local/share/jdtls"))
(defvar my/jdtls-workspace-root (expand-file-name "~/.cache/jdtls-workspace"))
(defvar my/jdtls-config-dir "config_linux")
(defvar my/jdtls-heap '("-Xms256m" "-Xmx2048m"))
(defvar my/java-common-imports
  '("java.util.*" "java.io.*" "java.nio.file.*" "java.util.stream.*"))

(defun my/jdtls-launcher-jar ()
  (car (directory-files (expand-file-name "plugins" my/jdtls-dir) t
                        "org\\.eclipse\\.equinox\\.launcher_.*\\.jar\\'")))

(defun my/jdtls-workspace ()
  (let* ((root (if-let ((project (project-current))) (project-root project) default-directory))
         (name (file-name-nondirectory (directory-file-name root))))
    (expand-file-name name my/jdtls-workspace-root)))

(defun my/jdtls-command (&rest _)
  `("java"
    "-Declipse.application=org.eclipse.jdt.ls.core.id1"
    "-Dosgi.bundles.defaultStartLevel=4"
    "-Declipse.product=org.eclipse.jdt.ls.core.product"
    ,@my/jdtls-heap
    "--add-modules=ALL-SYSTEM"
    "--add-opens" "java.base/java.util=ALL-UNNAMED"
    "--add-opens" "java.base/java.lang=ALL-UNNAMED"
    "-jar" ,(my/jdtls-launcher-jar)
    "-configuration" ,(expand-file-name my/jdtls-config-dir my/jdtls-dir)
    "-data" ,(my/jdtls-workspace)))

(defun my/java-insert-common-imports ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^package .*;" nil t) (forward-line 1))
    (dolist (import my/java-common-imports)
      (insert (format "import %s;\n" import)))))

(defun my/java-compile-command ()
  (cond ((file-exists-p "pom.xml") "mvn compile")
        ((file-exists-p "build.gradle") "gradle build")
        (t "javac *.java")))

(defun my/java-setup ()
  (setq-local compile-command (my/java-compile-command)
              c-basic-offset my/tab-width))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((java-mode java-ts-mode) . my/jdtls-command)))

(dolist (hook '(java-mode-hook java-ts-mode-hook))
  (add-hook hook #'eglot-ensure)
  (add-hook hook #'my/java-setup))

(with-eval-after-load 'cc-mode
  (define-key java-mode-map (kbd "C-c i") #'my/java-insert-common-imports))

(provide 'my-java)
;;; my-java.el ends here
