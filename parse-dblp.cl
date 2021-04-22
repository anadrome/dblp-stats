;; extract some DBLP authorship info from the XML dump to TSV
;; Produces four output output files, the first three joinable on paper_key
;;   papers.tsv - paper_key num_authors title venue_key venue_name year
;;   authors.tsv - paper_key author_name
;;   urls.tsv - paper_key url
;;   aliases.tsv - canonical alias
;; mjn, 2017-2021

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
(load "util.cl")

(defparameter *papers-file* (open #p"papers.tsv" :direction :output :if-exists :supersede))
(print-tsv '("paper_key" "num_authors" "title" "venue_key" "venue_name" "year") *papers-file*)
(defparameter *authors-file* (open #p"authors.tsv" :direction :output :if-exists :supersede))
(print-tsv '("paper_key" "author_name") *authors-file*)
(defparameter *urls-file* (open #p"urls.tsv" :direction :output :if-exists :supersede))
(print-tsv '("paper_key" "url") *urls-file*)
(defparameter *aliases-file* (open #p"aliases.tsv" :direction :output :if-exists :supersede))
(print-tsv '("canonical" "alias") *aliases-file*)

(defun process-pub (entry)
  (let ((key (get-attribute entry "key"))
        (data (plistify (cddr entry))))
    (let ((authors (getf-all data :author))
          (title (getf data :title))
          (venue (or (getf data :journal) (getf data :booktitle)))
          (year (getf data :year))
          (key-prefix (subseq key 0 (position #\/ key :from-end t)))
          (urls (getf-all data :ee)))
      (when (not (string= key-prefix "dblpnote")) ; there are a few test & error entries named this way
        (print-tsv (list key (length authors) title key-prefix venue year) *papers-file*)
        (dolist (author authors)
          (print-tsv (list key author) *authors-file*))
        (dolist (url urls)
          (print-tsv (list key url) *urls-file*))))))

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
          while entry
          do (if (equal (car entry) "www")
               (process-www entry)
               (process-pub entry)))))
(close *papers-file*)
(close *authors-file*)
(close *aliases-file*)
