;;; scalai.el -*- lexical-binding: t; -*-
;;; Code:

(require 'dash)
(require 'subr-x)
(require 's)
(require 'text-util)

;;;###autoload
(defun scalai--concat-imports (imports)
  "IMPORTS is a batch of scala import string."
  (let ((sep-imps (split-string imports "\n")))
    (thread-last
      sep-imps
      (-map (lambda (row)
              (let ((splitted (split-string row "\\."))
                    (c-name))
                (setq c-name (string-replace "}" "" (string-replace "{" "" (-last 'identity splitted))))
                (list (string-join (-drop-last 1 splitted) ".") (-map 's-trim (split-string c-name ","))))))
      (-group-by 'car)
      (-filter (lambda (coll) (not (string-empty-p (car coll)))))
      (-map (lambda (coll)
              (let ((key (car coll))
                    (vals (-sort 'string< (-distinct (-flatten (-map 'last (cdr coll))))))
                    (concated-vals))
                (setq concated-vals (cond
                                     ((-any (lambda (el) (s-equals? el "_")) vals) "_")
                                     ((or (> (-count 'identity vals) 1)
                                            (-filter (lambda (s) (s-contains? "," s)) vals)
                                            (-filter (lambda (s) (s-contains? "=>" s)) vals))
                                      (concat "{" (string-join vals ", ") "}"))
                                     (t (string-join vals ", "))))
                (concat key "." concated-vals))))
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
          (end))
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
             (concated (scalai--concat-imports imports)))
        (when (not (s-equals? imports concated))
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
  (when (s-matches? "\\w+(.+)\\(:\s.+\\)?" str)
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
       ((s-matches? "^\s+def\s+\\w+(.+)" line) (scalai--def-sep-args))
       ((s-matches? "^.+\s+class\s+\\w+(.+)" line) (scalai--class-sep-args))
       (t (message "Unrecognized current line."))))))

(provide 'scalai)
;;; scalai.el
