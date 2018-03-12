;; some mostly XML-related utilities
;; mjn, 2017-2018

; adapted from klacks:find-element, but looks for more than one possible lname,
; stopping at the first one of the set found
(defun find-element-multi (source &optional lnames uri)
  (loop
    (multiple-value-bind (key current-uri current-lname current-qname)
        (klacks:peek source)
      (case key
        ((nil)
         (return nil))
        (:start-element
          (when (and (eq key :start-element)
                     (or (null lnames)
                         (member current-lname lnames :test #'equal))
                     (or (null uri)
                         (equal uri current-uri)))
            (return
              (values key current-uri current-lname current-qname)))))
      (klacks:consume source))))

(defun get-attribute (xmls attribute)
  (cadr (assoc attribute (cadr xmls) :test #'equal)))

(defun getf-all (plist key)
  (loop for (k v) on plist by #'cddr
        if (string= k key)
        collect v))

(defun keywordify (string)
  (intern (string-upcase string) 'keyword))

(defun plistify (xmls-children)
  (mapcan (lambda (x) (list (keywordify (first x)) (third x)))
          (remove-if-not #'consp xmls-children)))

(defun print-tsv (row stream)
  (if row
    (let ((*print-pretty* nil))
      (princ (car row) stream)
      (dolist (e (cdr row))
        (write-char #\tab stream)
        (princ e stream))
      (write-char #\newline stream))))

