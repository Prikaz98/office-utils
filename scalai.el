;;; scalai.el -*- lexical-binding: t; -*-
;;; Code:

(require 'dash)
(require 'subr-x)
(require 's)

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
                (setq concated-vals (if (or (> (-count 'identity vals) 1)
                                            (-filter (lambda (s) (s-contains? "," s)) vals)
                                            (-filter (lambda (s) (s-contains? "=>" s)) vals))
                                        (concat "{" (string-join vals ", ") "}")
                                      (string-join vals ", ")))
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

(provide 'scalai)
;;; scalai.el
