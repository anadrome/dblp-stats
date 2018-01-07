;; extract some DBLP authorship info from the XML dump to TSV
;; mjn, 2017-2018

; FIXME: can't use venue name as unique key bc some are duplicated, e.g. there
; are several ICCC. probably need both key *and* name, so we can use key as
; unique but use name to filter out unwanted stuff in cases where we don't want
; everything that's under a given key (e.g. workshops). (can we just skip this
; last bit of filtering and either include everything under a key or exclude
; the key entirely?)

(ql:quickload :external-program)
(ql:quickload :cxml)
(ql:quickload :cxml-klacks)
(ql:quickload :cxml-xml)

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

(defvar *venue-keys* (make-hash-table :test #'equal))
(defparameter *dblp-authors-file* (open #p"dblp-authors.tsv" :direction :output :if-exists :supersede))

(defun process-pub (entry)
  (let ((key (cadr (assoc "key" (cadr entry) :test #'equal)))
        (data (plistify (cddr entry))))
    (let ((authors (getf-all data :author))
          (venue (or (getf data :journal) (getf data :booktitle)))
          (year (getf data :year))
          (key-prefix (subseq key 0 (position #\/ key :from-end t))))
      (when (and year (<= (parse-integer year) +end-year+))
        (let ((*print-pretty* nil))
          (dolist (author authors)
            (format *dblp-authors-file* "~a~c~a~c~a~%" venue #\tab author #\tab year)))
        (setf (gethash (list key-prefix venue) *venue-keys*) t)))))

(defun get-next-pub (source)
  (if (find-element-multi source '("article" "inproceedings"))
    (klacks:serialize-element source (cxml-xmls:make-xmls-builder))))

(let ((dblp-gz (external-program:process-output-stream
                 (external-program:start "gzcat" '("dblp.xml.gz") :output :stream))))
  (klacks:with-open-source (dblp (cxml:make-source dblp-gz))
    (loop for entry = (get-next-pub dblp)
          do (process-pub entry)
;          repeat 500000
          while entry)))
(close *dblp-authors-file*)

(with-open-file (venue-keys-file #p"venue-keys.tsv" :direction :output :if-exists :supersede)
  (let ((*print-pretty* nil))
    (alexandria:maphash-keys (lambda (x)
                               (let ((key (first x))
                                     (venue (second x)))
                                 (format venue-keys-file "~a~c~a~%" key #\tab venue)))
                             *venue-keys*)))
