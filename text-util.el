;;; text-util.el -*- lexical-binding: t; -*-
;;; package --- Summary
;;; Commentary:
;;;   Functions which help work with text
;;;
;;; Code:

(require 'dash)
(require 'subr-x)

;;autoload
(defun text-util-string-contains? (str1 str2 &optional ignore-case)
  "Search STR2 in STR1."
  (with-temp-buffer
    (insert str1)
    (goto-char (point-min))
    (let ((case-fold-search ignore-case))
      (ignore-error 'search-failed
	(search-forward str2)
	t))))

;;autoload
(defun text-util--string-is-capitalized (str)
  "Return STR is capitalized boolean value."
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]*\\'" str)))

;;autoload
(defun text-util-from-camel-case (str)
  "STR transform from camel case to snake case."
  (let ((arrstr (string-to-list str)))
    (->> (cdr arrstr)
         (-map 'char-to-string)
         (-map (lambda (chr)
                 (if (text-util--string-is-capitalized chr)
                     (concat "_" (downcase chr))
                   chr)))
         (cons (downcase (char-to-string (car arrstr))))
         (string-join))))

;;autoload
(defun text-util-from-camel-case-range ()
  "Get current range and transform it from camel case to snake case."
  (interactive)
  (let* ((start-point (region-beginning))
        (end-point (region-end))
        (curr-str (buffer-substring start-point end-point)))
    (kill-region start-point end-point)
    (insert (text-util-from-camel-case curr-str))))

(defun text-util-replace-in-whole-buffer (rgx newtxt)
  "Replace RGX to NEWTXT in whole buffer."
  (goto-char (point-min))
  (while (re-search-forward rgx nil t)
    (replace-match newtxt)))

(defun text-util-current-line ()
  "Return current line with 'save-excursion'."
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (buffer-substring-no-properties start end))))

(provide 'text-util)
;;; text-util.el
