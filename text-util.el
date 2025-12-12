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

(defun text-util-snake-to-kebab (str)
  (with-temp-buffer
    (insert str)
    (replace-string "_" "-" nil (point-min) (point-max))
    (buffer-string)))

(defun text-util-kebab-to-camel (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward "-" (point-max) t)
      (backward-delete-char 1)
      (upcase-char 1))
    (buffer-string)))

(defun text-util-camel-to-snake (str)
  (with-temp-buffer
    (insert str)
    (goto-char (+ (point-min) 1))
    (let ((case-fold-search nil))
      (while (search-forward-regexp "[[:upper:]]" nil t)
      (when (< (point) (point-max))
        (backward-char 1)
        (insert "_")
        (downcase-region (point) (+ (point) 1)))))
    (buffer-string)))

(text-util-camel-to-snake (text-util-kebab-to-camel (text-util-snake-to-kebab "snake_case")))
(text-util-camel-to-snake "textUtilToggleCase")

(defun text-util-toggle-case ()
  (interactive)
  (save-excursion
    (let ((bounds)
          (current-sexp)
          (toggled-case))
      (setq bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                    (bounds-of-thing-at-point 'sexp)))
      (setq current-sexp (buffer-substring-no-properties (car bounds) (cdr bounds)))
      (setq toggled-case (cond
                          ((s-contains? "_" current-sexp) (text-util-snake-to-kebab current-sexp))
                          ((s-contains? "-" current-sexp) (text-util-kebab-to-camel current-sexp))
                          (t (text-util-camel-to-snake current-sexp))))
      (replace-string current-sexp toggled-case nil (car bounds) (cdr bounds)))))

(provide 'text-util)
;;; text-util.el
