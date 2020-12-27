;; extract some DBLP authorship info from the XML dump to TSV
;; mjn, 2017-2020

; TODO: Still issues around what can be used as a venue key. Probably need
; both key *and* name, so we can use key as unique but use name to filter out
; unwanted stuff in cases where we don't want everything that's under a given
; key (e.g. workshops).
;  -- look at journals/jmlr as an example, seems like some kind of proceedings
;     ended up filed under there in addition to the actual journal

; TODO: make this callable as a script

(ql:quickload :external-program)
(ql:quickload :cxml)
(ql:quickload :cxml-klacks)
(ql:quickload :uiop)
(load "util.cl")

(defparameter *dblp-authors-file* (open #p"dblp-authors.tsv" :direction :output :if-exists :supersede))
(print-tsv '("name" "key" "venue_key" "venue_name" "year" "fraction") *dblp-authors-file*)
(defparameter *aliases-file* (open #p"aliases.tsv" :direction :output :if-exists :supersede))
(print-tsv '("canonical" "alias") *aliases-file*)
(defparameter *doi-file* (open #p"doi.tsv" :direction :output :if-exists :supersede))
(print-tsv '("key" "doi") *doi-file*)

(defun process-pub (entry)
  (let ((key (get-attribute entry "key"))
        (data (plistify (cddr entry))))
    (let ((authors (getf-all data :author))
          (venue (or (getf data :journal) (getf data :booktitle)))
          (year (getf data :year))
          (key-prefix (subseq key 0 (position #\/ key :from-end t)))
          (doi (find-if (lambda (url) (uiop:string-prefix-p "https://doi.org" url)) (getf-all data :ee))))
      (when (and authors year)
        (let ((fraction (/ 1.0 (length authors))))
          (dolist (author authors)
            (print-tsv (list author key key-prefix venue year fraction) *dblp-authors-file*))))
      (when doi
        (print-tsv (list key doi) *doi-file*)))))

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
          while entry)))
(close *dblp-authors-file*)
(close *aliases-file*)
(close *doi-file*)
