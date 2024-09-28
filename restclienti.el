;;; functions/restclienti.el -*- lexical-binding: t; -*-

(require 'json)
(require 'jsoni)

(defun restclienti--is-heading? ()
  "Is current line heading of the restclient entity."
  (save-excursion
    (let ((start)
          (end))
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (setq end (point))
    (string-match-p "^\\(?:POST\\|GET\\) http://.+$" (buffer-substring-no-properties start end)))))

(defun restclienti-collaps-current ()
  "Collaps body of current restclitn entity."
  (interactive)
  (save-excursion
    (when (and (restclienti--is-heading?) (re-search-forward "^\{$" (restclient-current-max) t))
    (let ((body-start)
          (body-end))
      (setq body-start (point))
      (re-search-forward "^\}$" (restclient-current-max))
      (setq body-end (point))
      (jsoni-minimalize-region body-start body-end)
      t))))

(defun restclienti-pretty-current ()
  "Pretty print body of current restclitn entity."
  (interactive)
  (save-excursion
    (let ((body-start)
          (body-end))
      (when (and (restclienti--is-heading?) (re-search-forward "^\{\.+\}$" (restclient-current-max) t))
        (setq body-end (point))
        (beginning-of-line)
        (setq body-start (point))
        (json-pretty-print body-start body-end)
        t))))

(defun restclienti-dwin ()
  "Do what i need.

Or collapse json body of rest time or pretty print"
  (interactive)
  (or (restclienti-pretty-current)
      (restclienti-collaps-current)))

(defun restclienti-step-forward (&optional count)
  "Go to the next restclient entity and pretty pritn body if it exists.

COUNT - time to repeat function"
  (interactive "p")
  (dotimes (_ (or count 1))
    (forward-line)
    (re-search-forward "^\\(?:POST\\|GET\\) http://.+$")
    (beginning-of-line)))

(defun restclienti-step-backward (&optional count)
  "Go to the previous restclient entity and pretty pritn body if it exists.

COUNT - time to repeat function"
  (interactive "p")
  (dotimes (_ (or count 1))
    (re-search-backward "^\\(?:POST\\|GET\\) http://.+$")))

(provide 'restclienti)
;;; restclienti.el
