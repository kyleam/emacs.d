;; set up babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (sh . t)
   (python . t)
   (R . t)
   (emacs-lisp . t)
   (latex . t)))

;; don't ask for confirmation before running code
(setq org-confirm-babel-evaluate nil)

;; babel minted latex export
;; modified from
;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '((R "rcode")
        (sh "shcode")
        (python "pythoncode")))

;; (setq org-export-latex-minted-options
;;       '(("frame" "lines")
;;         ("fontsize" "\\scriptsize")
;;         ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun org-babel-python-leading-chomp (string)
  "Strip the leading python output characters from STRING

This is different from `org-babel-chomp' (where '>' and '.' are
added to the regex) because it considers a bit more of the
context. If only single characters are matched against, then
meaningful spaces are often deleted from the output. However, if
spaces are not trimmed at all, extra spaces creep into the
output (particularly when the output source line is indented). To
get around this, only leading spaces that are followed by a
non-space character are deleted. The cases that this will botch
should be fairly uncommon.

This (I think) is only relevant when the 0rg babel results
property is set to 'output' (not 'value')"
  (let ((regexp " *>+\\|\\( \\)*\\.+\\| \\w" ))
    (while (and (/= (length string) 0)
                (eq (string-match regexp string) 0))
      (setq string (substring string 1)))
    string))

;; This is from ob-python.el. I have altered the org-babel-chomp call to
;; deal with leading characters in the output
(defun org-babel-python-evaluate-session
  (session body &optional result-type result-params)
  "Pass BODY to the Python process in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (let* ((send-wait (lambda () (comint-send-input nil t) (sleep-for 0 5)))
         (dump-last-value
          (lambda
            (tmp-file pp)
            (mapc
             (lambda (statement) (insert statement) (funcall send-wait))
             (if pp
                 (list
                  "import pprint"
                  (format "open('%s', 'w').write(pprint.pformat(_))"
                          (org-babel-process-file-name tmp-file 'noquote)))
               (list (format "open('%s', 'w').write(str(_))"
                             (org-babel-process-file-name tmp-file 'noquote)))))))
         (input-body (lambda (body)
                       (mapc (lambda (line) (insert line) (funcall send-wait))
                             (split-string body "[\r\n]"))
                       (funcall send-wait))))
    ((lambda (results)
       (unless (string= (substring org-babel-python-eoe-indicator 1 -1) results)
         (org-babel-result-cond result-params
           results
           (org-babel-python-table-or-string results))))
     (case result-type
       (output
        (mapconcat
         #'(lambda (string) (org-babel-chomp
                             (org-babel-python-leading-chomp string)))
         (butlast
          (org-babel-comint-with-output
              (session org-babel-python-eoe-indicator t body)
            (funcall input-body body)
            (funcall send-wait) (funcall send-wait)
            (insert org-babel-python-eoe-indicator)
            (funcall send-wait))
          2) "\n"))
       (value
        (let ((tmp-file (org-babel-temp-file "python-")))
          (org-babel-comint-with-output
              (session org-babel-python-eoe-indicator nil body)
            (let ((comint-process-echoes nil))
              (funcall input-body body)
              (funcall dump-last-value tmp-file (member "pp" result-params))
              (funcall send-wait) (funcall send-wait)
              (insert org-babel-python-eoe-indicator)
              (funcall send-wait)))
          (org-babel-eval-read-file tmp-file)))))))
