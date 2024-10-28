;;; jsoni.el -*- lexical-binding: t; -*-
;;; Code:
(require 'dash)
(require 'text-util)
(require 'subr-x)

(defun json--special-chars-replace ()
  "Create list of pattern to replace of special json symbols.

Returns list of pair pattern to replace."
  (->> (list "\"" "{" "}" "\\[" "]" "," ":")
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
  (->> (json--special-chars-replace)
       (-map (apply-partially 'apply 'text-util-replace-in-whole-buffer))))

(defun jsoni-minimalize-region (&optional beg end)
  "Minimize json in region.
BEG - beginning of region
END - ending of region"
  (interactive)
  (let* ((start (or beg (region-beginning)))
         (fin (or end (region-end)))
         (content (buffer-substring-no-properties start fin))
         (result (with-temp-buffer
                   (insert content)
                   (jsoni-minimalize)
                   (buffer-string))))
    (delete-region start fin)
    (insert result)))

(provide 'jsoni)
;;; scalai.el
