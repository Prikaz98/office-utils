;;; text-util.el -*- lexical-binding: t; -*-
;;; package --- Summary
;;; Commentary:
;;;   Functions which help work with text
;;;
;;; Code:

(require 'dash)
(require 'subr-x)

;;autoload
(defun text-util-string-is-capitalized (str)
  "Return STR is capitalized boolean value."
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]*\\'" str)))

;;autoload
(defun text-util-from-camel-case (str)
  "STR transform from camel case to snake case."
  (let ((arrstr (string-to-list str)))
    (thread-last
      (cdr arrstr)
      (-map 'char-to-string)
      (-map (lambda (chr)
              (if (text-util-string-is-capitalized chr)
                  (concat "_" (downcase chr))
                chr)))
      (cons (downcase (char-to-string (car arrstr))))
      (string-join))))

;;autoload
(defun text-util-from-camel-case-range ()
  "Get current range and transform it from camel case to snake case."
  (interactive)
  (let ((start-point)
        (end-point)
        (curr-str))
    (setq start-point (region-beginning))
    (setq end-point (region-end))
    (setq curr-str (buffer-substring start-point end-point))
    (kill-region start-point end-point)
    (insert (text-util-from-camel-case curr-str))))

(defun text-util-replace-in-whole-buffer (rgx newtxt)
  "Replace RGX to NEWTXT in whole buffer."
  (goto-char (point-min))
  (while (re-search-forward rgx nil t)
    (replace-match newtxt)))

(provide 'text-util)
;;; text-util.el
