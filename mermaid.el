;;; mermaid.el -*- lexical-binding: t; -*-
;;; package --- Summary
;;; Commentary:
;;;
;;; Code:

(require 'text-util)

(defun mermaid-to-plant-uml ()
  "Transform mermaid diagram to plant-uml.

Usually need to convert mermaid diagrams that
written in Emacs using mermaid.js to confluence."
  (interactive)
  (let ((diagram (buffer-substring-no-properties (region-beginning) (region-end))))
    (kill-new
     (with-temp-buffer
       (insert diagram)
       (goto-char (point-min))
       (delete-line)
       (text-util-replace-in-whole-buffer "and " "else ")
       (buffer-string)))))

(provide 'mermaid)
;;; mermaid.el
