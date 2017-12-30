;; analyze DBLP venue authors
;; mjn, 2017

(ql:quickload :external-program)
(ql:quickload :cxml)
(ql:quickload :cxml-klacks)
(ql:quickload :cxml-xml)

(defvar *stats* (make-hash-table :test #'equal))

(defconstant +end-year+ 2017)

;; utilities

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

(defun getf-all (plist key)
  (loop for (k v) on plist by #'cddr
        if (string= k key)
        collect v))

(defun keywordify (string)
  (intern (string-upcase string) 'keyword))

(defun plistify (xmls-children)
  (mapcan (lambda (x) (list (keywordify (first x)) (third x)))
          (remove-if-not #'consp xmls-children)))

;; process dblp.xml.gz

(defun process-pub (entry)
  (let ((type (keywordify (car entry)))
        (data (plistify (cddr entry))))
    ; TODO: probably want to keep 'type' somewhere as annotation of whether journal or conf for later display
    (let ((authors (mapcar #'intern (getf-all data :author)))
          (venue (intern (or (getf data :journal) (getf data :booktitle))))
          (year (parse-integer (getf data :year))))
      (if (<= year +end-year+)
        (dolist (author authors)
          (incf (gethash (list author venue year) *stats* 0)))))))

(defun get-next-pub (source)
  (if (find-element-multi source '("article" "inproceedings"))
    (klacks:serialize-element source (cxml-xmls:make-xmls-builder))))

(defun print-as-tsv (metadata count)
  (let ((*print-pretty* nil))
    (dolist (m metadata)
      (princ m)
      (write-char #\tab))
    (princ count)
    (write-char #\newline)))

(let ((dblp-gz (external-program:process-output-stream
                 (external-program:start "gzcat" '("dblp.xml.gz") :output :stream))))
  (klacks:with-open-source (dblp (cxml:make-source dblp-gz))
    (loop for entry = (get-next-pub dblp)
          do (process-pub entry)
          repeat 100000
          while entry)))

(with-open-file (*standard-output* "dblp-authors.tsv" :direction :output :if-exists :supersede)
  (maphash #'print-as-tsv *stats*))
