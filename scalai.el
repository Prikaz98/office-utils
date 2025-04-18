;;; scalai.el -*- lexical-binding: t; -*-
;;; Code:

(require 'dash)
(require 'subr-x)
(require 'text-util)

(defvar scalai-function-args-indention 4
  "Count of spaces which put when def transformed to separate lines args.")

(defvar scalai-ignore-import-list nil
  "List of imports which need to ignore everywhere.
format: (\"package.ClassName\")")

(defcustom scalai-cache-file
  (expand-file-name "scalai.cache" user-emacs-directory)
  "The name of Scalai's cache file."
  :group 'scalai
  :type 'string)

(defcustom scalai-import-search-method 'grep
  "Method to search imports.

Recommended to use 'rg' or 'git-grep' because their skip .gitignore files
You can install ripgrep by following link https://burntsushi.net/ripgrep/#installation"
  :group 'scalai
  :type '(radio
          (const :tag "Find - Grep" grep)
          (const :tag "Git grep" git-grep)
          (const :tag "Ripgrep" rg)))

(defcustom scalai-completion-system 'auto
  "The completion system to be used by Scalai."
  :group 'scalai
  :type '(radio
          (const :tag "Auto-detect" auto)
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(defcustom scalai-cache-imports nil
  "Enable cache file for imports.

Usefull when you use 'scalai-import-search-method' with 'grep' method"
  :group 'scalai
  :type 'boolean)

(defun scalai--find-root ()
  (if (fboundp 'project-root)
      (project-root (project-current t))
    (user-error "require `project` 0.3.0 or greater")))

(defun scalai--is-in-use? (path name-of-class file-content)
  "Try to find NAME-OF-CLASS in FILE-CONTENT.

Check also PATH.NAME-OF-CLASS in list to ignore."
  (or
   (not file-content)
   (progn
     (when (text-util-string-contains? name-of-class "=>")
       (text-util-string-contains? file-content (cadr (split-string name-of-class "=>")))))
   (string= name-of-class "_")
   (-contains? scalai-ignore-import-list (concat (string-trim (string-replace "import" "" path)) "." name-of-class))
   (text-util-string-contains? file-content name-of-class)))

(defun scalai--concat-import-vals (vals)
  "Consume VALS list and return concated import body."
  (cond
   ((not vals) "")
   ((or (> (-count 'identity vals) 1)
  (-filter (lambda (s) (text-util-string-contains? s ",")) vals)
  (-filter (lambda (s) (text-util-string-contains? s "=>")) vals))
    (concat "{" (string-join vals ", ") "}"))
   (t (string-join vals ", "))))

(defun scalai--concat-imports (imports &optional content)
  "IMPORTS is a batch of scala import string.

CONTENT is the rest of the file"
  (let ((sep-imps (split-string imports "\n")))
    (->> sep-imps
   (-map (lambda (row)
     (let ((splitted (split-string row "\\."))
           (c-name))
       (setq c-name (string-replace "}" "" (string-replace "{" "" (-last 'identity splitted))))
       (list (string-join (-drop-last 1 splitted) ".") (-map 'string-trim (split-string c-name ","))))))
   (-group-by 'car)
   (-filter (lambda (coll) (not (string-empty-p (car coll)))))
   (-map (lambda (coll)
     (let* ((path (car coll))
      (vals (let ((sorted (-sort 'string< (-distinct (-flatten (-map 'last (cdr coll)))))))
        (if (string= (car sorted) "_")
            (append (cdr sorted) '("_"))
          sorted)))
      (in-use (-filter (lambda (el) (scalai--is-in-use? path el content)) vals))
      (unuse (-filter (lambda (el) (not (scalai--is-in-use? path el content))) vals)))
       (concat
        (if in-use (concat path "." (scalai--concat-import-vals in-use)) "")
        (if (and in-use unuse) "\n" "")
        (if unuse (concat "//" path "." (scalai--concat-import-vals unuse)) "")))))
   (-sort 'string<)
   ((lambda (coll) (string-join coll "\n"))))))

(defun scalai--concat-imports-region ()
  "Concat separeted imports to one in region."
  (save-excursion
    (let ((imports (buffer-substring-no-properties (region-beginning) (region-end)))
    (concated))
      (setq concated (scalai--concat-imports imports))
      (delete-region (region-beginning) (region-end))
      (insert (concat concated "\n")))))

(defun scalai--concat-imports-automaticly (&optional force)
  "Concat separeted imports to one.

Automatically determines strings of imports which need to concat"
  (save-excursion
    (let ((start)
          (end)
          (check (when (not force) (string= "y" (read-string "Comment unused imports?y/n (default n) ")))))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp "^import" nil t)
          (beginning-of-line)
          (setq start (point))
          (while (progn
                   (forward-line)
                   (string= (current-word) "import")))
          (end-of-line)
          (setq end (point))
          (let* ((imports (buffer-substring-no-properties start end))
                 (content (when check (buffer-substring-no-properties end (point-max))))
                 (concated (scalai--concat-imports imports content)))
            (when (not (string= imports concated))
              (delete-region start end)
              (goto-char start)
              (insert (concat concated "\n")))))))))

(defun scalai-pretty-imports (&optional force)
  "Pretty Scala imports.

Might be called by region or evaluate imports automatically."
  (interactive)
  (if (use-region-p)
      (scalai--concat-imports-region)
    (scalai--concat-imports-automaticly force)))

(defun scalai--evaluate-current-indent ()
  "Return whitespace of current line."
  (save-excursion
    (let ((start)
    (end)
    (space))
      (beginning-of-line)
      (setq start (point))
      (forward-word)
      (backward-word)
      (setq end (point))
      (setq space (buffer-substring start end))
      space)))

(defun scalai--args-to-sep-line (str)
  "STR convert to seporate line def."
  (when (string-match "\\w+(.+)\\(:\s.+\\)?" str)
    (let ((arg-indent (let ((agg ""))
      (dotimes (_ scalai-function-args-indention)
        (setq agg (concat agg " ")))
      agg)))
      (with-temp-buffer
  (insert str)
  (let ((indent (scalai--evaluate-current-indent)))
    (text-util-replace-in-whole-buffer "\\(\s+\\)?:" ":")
    (text-util-replace-in-whole-buffer ":\\(\s+\\)?" ": ")
    (text-util-replace-in-whole-buffer "\\(\\w+\\)(\\(\s+\\)?" (concat "\\1(\n" indent arg-indent))
    (while (re-search-forward "\\w+\\( +\\)?:\\( +\\)\\w+\\(\\[.+]\\)?," nil t)
      (goto-char (- (point) 1))
      (when (= ?\, (char-after (point)))
        (goto-char (+ 1 (point)))
        (while (= ?\s (char-after (point)))
    (delete-char 1))
        (insert (concat "\n" indent arg-indent))))
    (goto-char (point-max))
    (re-search-backward ")")
    (replace-match (concat "\n" indent ")")))
  (buffer-string)))))

(defun scalai--put-instead (content start end)
  "Put CONTENT instad of text from START to END.

Remove without modifying kill ring."
  (delete-region start end)
  (insert content))

(defun scalai--def-sep-args ()
  "Return nil if format of string doesn't support."
  (let* ((start (progn (beginning-of-line) (point)))
   (end (progn (re-search-forward "\s+=\s+") (point)))
   (aligned (scalai--args-to-sep-line (buffer-substring-no-properties start end))))
    (if aligned
  (scalai--put-instead aligned start end)
      (message "Couldn't align current def"))
    aligned))

(defun scalai--class-sep-args ()
  "Return nil if format of string doesn't support."
  (let* ((start (progn (beginning-of-line) (point)))
   (end (progn (end-of-line) (re-search-backward ")") (+ 1 (point))))
   (aligned (scalai--args-to-sep-line (buffer-substring-no-properties start end))))
    (if aligned
  (scalai--put-instead aligned start end)
      (message "Couldn't align current class"))
    aligned))

(defun scalai-sep-args ()
  "Transefer current def/class args to seporate lines."
  (interactive)
  (save-excursion
    (let ((line (text-util-current-line)))
      (cond
       ((string-match "^\\(\s+\\)?\\(\\(override\\|private\\|protected\\)\s+?\\)?def\s+\\w+(.+)" line) (scalai--def-sep-args))
       ((string-match "^\\(.+\s+\\)?class\s+\\w+(.+)" line) (scalai--class-sep-args))
       (t (message "Unrecognized current line."))))))

(defun scalai-completing-read (prompt choices &optional initial-input)
  "Present a scalai tailored PROMPT with CHOICES."
  (let ((prompt "Find import: "))
    (pcase (if (eq scalai-completion-system 'auto)
               (cond ((bound-and-true-p ido-mode)  'ido)
                     ((bound-and-true-p helm-mode) 'helm)
                     ((bound-and-true-p ivy-mode)  'ivy)
                     (t 'default))
             scalai-completion-system)
      ('default (completing-read prompt choices nil nil initial-input))
      ('ido (ido-completing-read prompt choices nil nil initial-input))
      ('helm
       (if (and (fboundp 'helm)
    (fboundp 'helm-make-source))
     (helm :sources
     (helm-make-source "Scalai" 'helm-source-sync
           :candidates choices
           :action #'identity)
     :prompt prompt
     :input initial-input
     :buffer "*helm-scalai*")
   (user-error "Please install helm")))
      ('ivy
       (if (fboundp 'ivy-read)
     (ivy-read prompt choices
         :initial-input initial-input
         :action #'identity
         :caller 'scalai-completing-read)
   (user-error "Please install ivy")))
      (_ (user-error "Not found completing system")))))

;;STOLEN from projectile.el
(defun scalai-unserialize (filename)
  "Read data serialized by `scalai-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
  (insert-file-contents filename)
  ;; this will blow up if the contents of the file aren't
  ;; lisp data structures
  (read (buffer-string))))))

;;STOLEN from projectile.el
(defun scalai-serialize (project-root data filename)
  (if (file-writable-p filename)
      (with-temp-file filename
  (let ((cache (assoc-delete-all project-root (scalai-unserialize filename))))
    (insert (let (print-length) (prin1-to-string (cons (list project-root data) cache))))))
    (message "Scalai cache '%s' not writeable" filename)))

;;TODO: support for java also
(defun scalai--eval-imports ()
  "Collect imports in project"
  (let* ((cmd (pcase scalai-import-search-method
                ('rg "rg -i --no-config --no-filename --type=scala -e '^import' . | sed 's/^\s*//g' | sort | uniq")
                ('grep "find . -name '*.scala' -exec grep '^import' {} \\; | sed 's/^\s*//g' | sort | uniq")
                ('git-grep "git grep -h '^import' -- '*.scala' | sed 's/^\s*//g' | sort | uniq")
                (_ (user-error "Not found scala import search method"))))
         (shell-output (with-temp-buffer
                         (shell-command cmd t "*scalai-find-imports-error*")
                         (buffer-string))))
    (->> (split-string (string-trim shell-output) "\n" t)
         (-map (lambda (row)
                 (let* ((splitted (split-string row "\\."))
                        (class-names (-map 'string-trim (split-string (string-replace "}" "" (string-replace "{" "" (-last 'identity splitted))) ",")))
                        (path (string-join (-drop-last 1 splitted) ".")))
                   (-map (lambda (class-name) (concat path "." class-name)) class-names))))
         (-flatten)
         (-filter (lambda (str) (not (text-util-string-contains? str "=>"))))
         (-distinct))))

(defun scalai--eval-imports-cache ()
  "Read cached imports or evaluate new."
  (let ((default-directory (scalai--find-root)))
    (if (not scalai-cache-imports)
        (scalai--eval-imports)
      (or (car (assoc-default default-directory (scalai-unserialize scalai-cache-file)))
          (let ((imports (scalai--eval-imports)))
            (scalai-serialize default-directory imports scalai-cache-file)
            imports)))))

(defun scalai-invalidate-cache ()
  "Remove cache file."
  (interactive)
  (let* ((default-directory (scalai--find-root))
   (cache (assoc-delete-all default-directory (scalai-unserialize scalai-cache-file))))
    (cond
     ((not default-directory) (message "Couldn't recognize project root"))
     ((not (file-writable-p scalai-cache-file)) (message "Scalai cache '%s' not writeable" scalai-cache-file))
     (t (with-temp-file scalai-cache-file
    (insert (let (print-length) (prin1-to-string cache))))))))

(defun scalai-find-import ()
  "Grep import in project and offer them to add in file."
  (interactive)
  (let* ((point-word (current-word))
   (imports (scalai--eval-imports-cache))
   (to-insert (scalai-completing-read "Find import: " imports point-word)))
    (save-excursion
      (unless (search-backward-regexp (concat "^" to-insert) nil t)
          (or (search-backward-regexp (concat "^" (car (split-string to-insert "\\."))) nil t)
              (search-backward-regexp "^import" nil t)
              (search-backward-regexp "^package" nil t)
              (goto-char (point-min)))
          (end-of-line)
          (insert (concat "\n" to-insert))))))

(provide 'scalai)
;;; scalai.el
