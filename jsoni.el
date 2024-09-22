;;; jsoni.el -*- lexical-binding: t; -*-
;;; Code:
(require 'dash)
(require 'text-util)
(require 'subr-x)

(defun json--special-chars-replace ()
  "Create list of pattern to replace of special json symbols.

Returns list of pair pattern to replace."
  (thread-last
    (list "\"" "{" "}" "\\[" "]" "," ":")
    (-map (lambda (char)
            (let ((to-char (string-replace "\\" "" char)))
              (list
               (concat "\\( +\\)" char) to-char
               (concat char "\\( +\\)") to-char
               (concat "\n" char) to-char
               (concat char "\n") to-char))))
    (-flatten)
    (-partition 2)))

;;;###autoload
(defun jsoni-minimalize ()
  "Minimize json in whole buffer."
  (interactive)
  (thread-last
    (json--special-chars-replace)
    (-map (apply-partially 'apply 'text-util-replace-in-whole-buffer))))

(provide 'jsoni)
;;; scalai.el
