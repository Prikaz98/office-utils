;;; scalai.el -*- lexical-binding: t; -*-
;;; Code:

(require 'dash)
(require 'subr-x)
(require 'text-util)

(defun scalai--is-in-use? (className fileContent)
  "Return boolean"
  (or
   (not fileContent)
   (progn
     (when (text-util-string-contains? "=>" className)
       (text-util-string-contains? (cadr (split-string className "=>")) fileContent)))
   (string= className "_")
   (text-util-string-contains? className fileContent)))

(defun scalai--concat-import-vals (vals)
  "Consume vals list and return concated import body"
  (cond
   ((not vals) "")
   ((or (> (-count 'identity vals) 1)
        (-filter (lambda (s) (text-util-string-contains? "," s)) vals)
        (-filter (lambda (s) (text-util-string-contains? "=>" s)) vals))
    (concat "{" (string-join vals ", ") "}"))
   (t (string-join vals ", "))))

;;;###autoload
(defun scalai--concat-imports (imports &optional content)
  "IMPORTS is a batch of scala import string.

Check if className is in content of file"
  (let ((sep-imps (split-string imports "\n")))
    (thread-last
      sep-imps
      (-map (lambda (row)
              (let ((splitted (split-string row "\\."))
                    (c-name))
                (setq c-name (string-replace "}" "" (string-replace "{" "" (-last 'identity splitted))))
                (list (string-join (-drop-last 1 splitted) ".") (-map 'string-trim (split-string c-name ","))))))
      (-group-by 'car)
      (-filter (lambda (coll) (not (string-empty-p (car coll)))))
      (-map (lambda (coll)
              (let* ((key (car coll))
                     (vals (-sort 'string< (-distinct (-flatten (-map 'last (cdr coll))))))
                     (in-use (-filter (lambda (el) (scalai--is-in-use? el content)) vals))
                     (unuse (-filter (lambda (el) (not (scalai--is-in-use? el content))) vals)))
                (concat
                 (if in-use (concat key "." (scalai--concat-import-vals in-use)) "")
                 (if unuse (concat "//" key "." (scalai--concat-import-vals unuse)) "")))))
      (-sort 'string<)
      ((lambda (coll) (string-join coll "\n"))))))

;;;###autoload
(defun scalai-concat-imports-region ()
  "Concat separeded imports to one in region."
  (interactive)
  (save-excursion
    (let ((imports (buffer-substring-no-properties (region-beginning) (region-end)))
          (concated))
    (setq concated (scalai--concat-imports imports))
    (kill-region (region-beginning) (region-end))
    (insert (concat concated "\n")))))

;;;###autoload
(defun scalai-concat-imports-automaticly ()
  "Concat separeded imports to one.

Automaticli determines strings of imports which need to concat"
  (interactive)
  (save-excursion
    (let ((start)
          (end)
          (check (string= "y" (read-string "Comment unused imports?y/n (default n) "))))
      (save-excursion
        (goto-char (point-min))
        (search-forward "import")
        (beginning-of-line)
        (setq start (point))
        (while (progn
                 (forward-line)
                 (string= (current-word) "import")))
        (end-of-line)
        (setq end (point)))
      (let* ((imports (buffer-substring-no-properties start end))
             (content (when check (buffer-substring-no-properties end (point-max))))
             (concated (scalai--concat-imports imports content)))
        (when (not (string= imports concated))
          (kill-region start end)
          (goto-char start)
          (insert (concat concated "\n")))))))

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

(defvar scalai-function-args-indention 4
  "Count of spaces which put when def transformed to separate lines args.")

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
       ((string-match "^\s+def\s+\\w+(.+)" line) (scalai--def-sep-args))
       ((string-match "^.+\s+class\s+\\w+(.+)" line) (scalai--class-sep-args))
       (t (message "Unrecognized current line."))))))

(provide 'scalai)
;;; scalai.el
