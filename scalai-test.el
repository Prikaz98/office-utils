(require 'scalai)
(require 'dash)
(require 'cl-macs)

(defun scalai--read-test-case-file (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (let ((sep-case "//===================================================\n")
          (sep-params "//==>\n")
          (content (buffer-substring-no-properties (point-min) (point-max)))
          (test-cases nil))
      (->> (split-string content sep-case)
           (-map (lambda (test-case)
                   (let ((params (split-string test-case sep-params)))
                     (list
                      :in (replace-regexp-in-string  "\n$" "" (car params))
                      :out (replace-regexp-in-string  "\n$" "" (cadr params))))))))))


(defun scalai-test (fn test-file-name)
  (let ((test-cases (scalai--read-test-case-file test-file-name)))
    (-each
        test-cases
      (lambda (coll)
        (let ((in (plist-get coll :in))
              (out (plist-get coll :out)))
          (cl-assert (string= (funcall fn in) out) t))))))

(scalai-test #'scalai--args-to-sep-line "scalai-test-cases.scala")
(scalai-test #'scalai--concat-imports "scalai-imports-test-cases.scala")
