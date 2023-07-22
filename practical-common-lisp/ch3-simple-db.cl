(defvar *db* nil)

;;; cd operations
(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (read-line *query-io*))

(defun prompt-for-cd ()
    (make-cd
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
        (y-or-n-p "Ripped [y/n]: ")))

;;; database operations
(defun add-cds ()
    (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: "))
            (return))))

(defun add-record (cd)
    (push cd *db*))

(defun dump-db ()
    (dolist (cd *db*)
        (format t "~{~a:~10t~a~%~}~%" cd)))

(defun save-db (filename)
    (with-open-file (out filename
                        :direction :output
                        :if-exists :supersede)
        (with-standard-io-syntax
            (print *db* out))))

(defun load-db (filename)
    (with-open-file (in filename)
        (with-standard-io-syntax
            (setf *db* (read in)))))

;;; database select
(defun select-by-artist (artist)
    (remove-if-not
        #'(lambda (cd) (equal (getf cd :artist) artist))
        *db*))

"""
(defun where (&key title artist ratting (ripped nil ripped-p))
    #'(lambda (cd)
          (and
              (if title (equal (getf cd :title) title) t)
              (if artist (equal (getf cd :artist) artist) t)
              (if ratting (equal (getf cd :ratting) ratting) t)
              (if ripped-p (equal (getf cd :ripped) ripped) t))))
"""

(defun select (where-fn)
    (remove-if-not where-fn *db*))

;;; data base update
(defun update (select-fn &key title artist rating (ripped nil ripped-p))
    (setf *db*
        (mapcar
            #'(lambda (row)
                  (when (funcall select-fn row)
                      (if title (setf (getf row :title) title))
                      (if artist (setf (getf row :artist) artist))
                      (if rating (setf (getf row :rating) rating))
                      (if ripped-p (setf (getf row :ripped) ripped)))
                  row) *db*)))

;;;delete record(s) from db
(defun delete-rows (select-fn)
    (setf *db* (remove-if select-fn *db*)))

;;; use macro remove duplications
(defun make-comp-expr (field value)
    `(equal (getf cd ,field) ,value))

(defun make-comp-list (fields)
    (loop while fields
        collecting (make-comp-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
    `#'(lambda (cd) (and ,@(make-comp-list clauses))))
