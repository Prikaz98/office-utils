;;; jsoni.el -*- lexical-binding: t; -*-
;;; Code:

(require 'text-util)

;;;###autoload
(defun jsoni-minimalize ()
  "Minimize json in whole buffer."
  (interactive)
  (thread-last
    (list
     "^\\( +\\)\"" "\""
     "^\\( +\\)\\[" "\\["
     "^\\( +\\)]" "]"
     "{\n" "{"
     "\\[\n" "["
     "}\n" "}"
     "]\n" "]"
     "\,\n" "\,"
     "\"\n" "\""
     "\\( +\\):\\( +\\)" ":")
    (-partition 2)
    (-map (apply-partially 'apply 'text-util-replace-in-whole-buffer))))


(provide 'jsoni)
;;; scalai.el
