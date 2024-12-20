;;; jira.el -*- lexical-binding: t; -*-
;;; package --- Summary
;;; Commentary:
;;;   Functions which help use emacs even when you need to create stupid jira tasks via org files.
;;;
;;; Code:

(require 'dash)
(require 'subr-x)
(require 'text-util)

(defcustom jira-tasks-dir "~/tmp/"
  "Dir where change log files are stored."
  :group 'jira
  :type 'string)

(defun jira-create-new-task (title)
  "Create template file for tasks."
  (interactive "sEnter title: \n")
  (setq title (string-replace "/" "&" title))
  (find-file (concat jira-tasks-dir title ".org"))
  (insert (concat "#+title: " title "\n\n"))
  (insert "_Легенда:_\n\n_Что сделать?_"))

(defun jira--special-org-chars-convert ()
  "Create list of pattern to replace of special org symbols.

Returns list of pair pattern to replace."
  (->> (list "_" "+" "/" "_")
       (-partition 2)
       (-map (lambda (coll)
               (list
                (concat "^" (car coll)) (last coll)
                (concat "\s" (car coll)) (last coll)
                (concat (car coll) "\s") (last coll)
                (concat (car coll) "$") (last coll))))
       (-flatten)
       (-partition 2)))

(defun jira--org-link-to-jira-format (org-link)
  "Convert org link to jira link.

ORG-LINK - link if format [[http:example][name-link]]
Return link in format [name-link|http:example]"
  (let ((result))
    (setq result
          (->
            (split-string (string-replace "]]" ""  (string-replace "[[" "" org-link)) "\\]\\[")
            (reverse)
            (string-join "|")))
    (concat "\[" result "\]")))

(defun jira--org-links-convert ()
  "Convert org links to jira links."
  (goto-char (point-min))
  (let ((pattern "\\[\\[\\(.+\\)\\]\\]")
        (start)
        (end)
        (result))
    (when (re-search-forward pattern nil t)
      (goto-char (- (point) 1))
      (setq end (+ (point) 1))
      (search-backward "[[")
      (setq start (point))
      (setq result (jira--org-link-to-jira-format (buffer-substring-no-properties start end)))
      (kill-region start end)
      (insert result))))

(defun jira-from-org-kill ()
  "Convert org to jira format."
  (interactive)
  (let ((copy (buffer-string))
        (replace-list
         (->> (jira--special-org-chars-convert)
              (cons (list "^#\\+title\\(.+\\)$" ""))
              (cons (list "\\(\s+\\)?#\\+begin\\(.+\\)$" "{code:java}"))
              (cons (list "\\(\s+\\)?#\\+end\\(.+\\)$" "{code}"))
              (cons (list "^*+" "")))))
    (kill-new
     (with-temp-buffer
       (insert copy)
       (-each replace-list (apply-partially 'apply 'text-util-replace-in-whole-buffer))
       (jira--org-links-convert)
       (goto-char (point-min))
       (while (= ?\n (char-after (point)))
         (delete-char 1))
       (buffer-string)))))

(provide 'jira)
;;; jira.el
