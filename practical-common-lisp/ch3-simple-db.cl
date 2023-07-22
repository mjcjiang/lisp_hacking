(defvar *db* nil)

;;; cd operations
(defun make-cd (title artist ratting ripped)
    (list :title title :artist artist :ratting ratting :ripped ripped))

(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (read-line *query-io*))

(defun prompt-for-cd ()
    (make-cd
        (prompt-read "Title")
        (prompt-read "Artist")
        (or (parse-integer (prompt-read "Ratting") :junk-allowed t) 0)
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

;;(defun where (&key title artist ratting (ripped nil ripped-p))
