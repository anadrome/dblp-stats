;; extract some DBLP authorship info from the XML dump to TSV
;; mjn, 2017-2018

; FIXME: can't use venue name as unique key bc some are duplicated, e.g. there
; are several ICCC. probably need both key *and* name, so we can use key as
; unique but use name to filter out unwanted stuff in cases where we don't want
; everything that's under a given key (e.g. workshops). (can we just skip this
; last bit of filtering and either include everything under a key or exclude
; the key entirely?)
;  -- look at journals/jmlr as an example. is this stuff we should actually include?
;     or is there some other way to exclude it? seems to be proceedings stuff

; other TODOs: rename this from stats.cl, and make it callable as a proper script

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

;; process dblp.xml.gz

(defvar *venue-keys* (make-hash-table :test #'equal))

(defparameter *dblp-authors-file* (open #p"dblp-authors.tsv" :direction :output :if-exists :supersede))
(print-tsv '("venue" "name" "year") *dblp-authors-file*)
(defparameter *aliases-file* (open #p"aliases.tsv" :direction :output :if-exists :supersede))
(print-tsv '("canonical" "alias") *aliases-file*)

(defun process-pub (entry)
  (let ((key (get-attribute entry "key"))
        (data (plistify (cddr entry))))
    (let ((authors (getf-all data :author))
          (venue (or (getf data :journal) (getf data :booktitle)))
          (year (getf data :year))
          (key-prefix (subseq key 0 (position #\/ key :from-end t))))
      (when (and year (<= (parse-integer year) +end-year+))
        (dolist (author authors)
          (print-tsv (list venue author year) *dblp-authors-file*))
        (setf (gethash (list key-prefix venue) *venue-keys*) t)))))

(defun process-www (entry)
  (let* ((key (get-attribute entry "key"))
         (data (plistify (cddr entry)))
         (authors (getf-all data :author)))
    (let ((key-prefix (subseq key 0 (position #\/ key)))
          (author (car authors))
          (aliases (cdr authors)))
      (if (and aliases (string= key-prefix "homepages"))
        (dolist (alias aliases)
          (print-tsv (list author alias) *aliases-file*))))))

(defun get-next-pub (source)
  (if (find-element-multi source '("article" "inproceedings" "www"))
    (klacks:serialize-element source (cxml-xmls:make-xmls-builder))))

(let ((dblp-gz (external-program:process-output-stream
                 (external-program:start "gzcat" '("dblp.xml.gz") :output :stream))))
  (klacks:with-open-source (dblp (cxml:make-source dblp-gz))
    (loop for entry = (get-next-pub dblp)
          do (if (equal (car entry) "www")
               (process-www entry)
               (process-pub entry))
;          repeat 500000
          while entry)))
(close *dblp-authors-file*)
(close *aliases-file*)

(with-open-file (venue-keys-file #p"venue-keys.tsv" :direction :output :if-exists :supersede)
  (print-tsv '("key" "venue") venue-keys-file)
  (alexandria:maphash-keys (lambda (x)
                             (let ((key (first x))
                                   (venue (second x)))
                               (print-tsv (list key venue) venue-keys-file)))
                           *venue-keys*))
