(require 'scalai)
(require 'dash)
(require 'cl-macs)

(defun scalai--read-test-case-file (file-name)
  (with-current-buffer file-name
    (let ((sep-case "//===================================================\n")
          (sep-params "//==>\n")
          (content (buffer-substring-no-properties (point-min) (point-max)))
          (test-cases nil))
      (thread-last
        (split-string content sep-case)
        (-map (lambda (test-case)
                (let ((params (split-string test-case sep-params)))
                  (list
                    :in (s-replace-regexp  "\n$" "" (car params))
                    :out (s-replace-regexp  "\n$" "" (cadr params))))))))))

(defun scalai-sep-args-test ()
  (let ((test-cases (scalai--read-test-case-file "scalai-test-cases.scala")))
    (-each
        test-cases
      (lambda! ((&key in out))
        (cl-assert (string= (scalai--def-args-to-sep-line in) out) t)))))

(defun scalai-concat-imports-test ()
  (let ((test-cases (scalai--read-test-case-file "scalai-imports-test-cases.scala")))
    (-each
        test-cases
      (lambda! ((&key in out))
        (cl-assert (string= (scalai--concat-imports in) out) t)))))

(scalai-sep-args-test)
(scalai-concat-imports-test)
