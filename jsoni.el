;;; jsoni.el -*- lexical-binding: t; -*-
;;; Code:

(require 'text-util)

;;;###autoload
(defun jsoni-minimalize ()
  "Minimize json in whole buffer."
  (interactive)
  (text-util-replace-in-whole-buffer " " "")
  (text-util-replace-in-whole-buffer "\n" ""))

(provide 'jsoni)
;;; scalai.el
