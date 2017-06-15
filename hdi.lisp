;;;;
;;  Handwritten Digit Identifier
;;
;;  @author: Tyler St. Onge
;;  @date: 4.20.2017
;;  @file: hdi.lisp
;;

(load "~/quicklisp/setup.lisp")
(ql:quickload 'opticl)

;;;;
;; Global Variables
(defvar *image-size* 32)
(defvar *id-count* 0)
(defvar *threshold* 150)
(defvar *entry-threshold* 0.5)
(defvar *database* (make-hash-table))
(defvar *end-kernels* '(
                 (0 1 0 
                  0 1 0 
                  0 0 0) 
                 (0 0 1 
                  0 1 0 
                  0 0 0)
                 (0 0 0
                  0 1 1
                  0 0 0)
                 (0 0 0
                  0 1 0
                  0 0 1)
                 (0 0 0
                  0 1 0
                  0 1 0)
                 (0 0 0
                  0 1 0
                  1 0 0)
                 (0 0 0
                  1 1 0
                  0 0 0)
                 (1 0 0
                  0 1 0
                  0 0 0)))

;;;;
;; Generics so SBCL stops yelling at me

;; Handwritten Digit Methods 
(defgeneric display (hc))
(defgeneric load-digit (path))
(defgeneric filter (hd))
(defgeneric vertical-slice (hd))
(defgeneric horizontal-slice (hd))
(defgeneric create-handwritten-digit (path))
(defgeneric horizontal-slice-string (hd))
(defgeneric vertical-slice-string (hd))
(defgeneric thin-edges (hd))
(defgeneric thin-edges-1 (hd))
(defgeneric number-of-neighbors (pixels x y))
(defgeneric number-of-transitions (pixels x y))
(defgeneric count-ends (hd))

;; Display methods
(defgeneric display-raw-image (img))
(defgeneric pretty-display-raw-image (img))

;;;
;; Utility functions 

;; Get the next ID value, and increment ID counter
(defun get-id (&aux id)
  (setf id *id-count*)
  (setf *id-count* (+ *id-count* 1))
  id)

; takes much too long on larger inputs, do not use
;(defun levenshtein-distance-1 (s1 s1-length s2 s2-length &aux cost)
;  (setf cost 1)
;  (cond
;    ((= s1-length 0)
;     s2-length)
;    ((= s2-length 0)
;     s1-length)
;    (t
;      (if (equal (char s1 (- s1-length 1)) (char s2 (- s2-length 1)))
;        (setf cost 0))
;      (min 
;        (+ (levenshtein-distance-1 s1 (- s1-length 1) s2 s2-length) 1) 
;        (+ (levenshtein-distance-1 s1 s1-length s2 (- s2-length 1)) 1)
;        (+ (levenshtein-distance-1 s1 (- s1-length 1) s2 (- s2-length 1)) cost))))) 
;
;(defun levenshtein-distance-old (s1 s2)
;  (levenshtein-distance-1 s1 (length s1) s2 (length s2)))

(defun levenshtein-distance (str1 str2)
  (let ((n (length str1))
	(m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
	  ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
	  (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
	(setf (svref col 0) (1+ i))
	(dotimes (j m)
	  (setf (svref col (1+ j))
		(min (1+ (svref col j))
		     (1+ (svref prev-col (1+ j)))
		     (+ (svref prev-col j)
			(if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
	(rotatef col prev-col))
      (svref prev-col m))))

;; Pad images for nicer output
(defun pad-value (n)
  (cond
    ((< n 10) (format nil "00~A" n))
    ((< n 100) (format nil "0~A" n))
    (t (format nil "~A" n))))

;; Remove all similar elements that are neighbors within a list
(defun remove-similar-neighbors (lst &aux previous nlst)
  (setf previous (car lst))
  (setf nlst (cons previous nlst))
  (loop for el in lst do
        (if (not (equal previous el))
          (let ()
            (setf previous el)
            (setf nlst (cons el nlst)))))
  nlst)

;; Returns the list of elements passed through the second parameter, with the first parameter appended to the end.
(defun snoc (head tail)
    (append tail (cons head ())))

;; Write a list to a string
(defun numlist-to-string (lst)
  (when lst
    (concatenate 'string (write-to-string (car lst)) (numlist-to-string (cdr lst)))))

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;;;;
;; Handwritten Digit 

(defclass handwritten-digit ()
  ((id 
     :accessor handwritten-digit-id 
     :initarg :id)
   (pixels 
     :accessor handwritten-digit-pixels 
     :initarg :pixels)
   (vertical-slice 
     :accessor handwritten-digit-vertical-slice 
     :initarg :vertical-slice)
   (horizontal-slice
     :accessor handwritten-digit-horizontal-slice
     :initarg :horizontal-slice)
   (end-counts
     :accessor handwritten-digit-end-counts
     :initarg :end-counts)))

;; Create and manipulate the digit into a useable form
(defmethod create-handwritten-digit ((path string) &aux hd)
  (setf hd (load-digit path))
  (filter hd)
  (thin-edges hd)
  (horizontal-slice hd)
  (vertical-slice hd)
  (count-ends hd)
  hd)

;; Display a digit 
(defmethod display ((hc handwritten-digit))
  (format t "-- Handwritten Digit--~%")
  (format t "ID: ~A~%" (handwritten-digit-id hc))
  (format t "Pixels:~%")
  (dotimes (y *image-size*)
    (dotimes (x *image-size*)
      (format t " ~A " (pad-value (aref (handwritten-digit-pixels hc) x y 0))))
    (format t "~%")))

;; Load an image of a digit
(defmethod load-digit ((path string) &aux img)
  (setf img (opticl:read-png-file path))
  (make-instance 'handwritten-digit :id (get-id) :pixels img))

;; Filter the matrix of pixels from the image to 0 if above threshold, and 1 if below
(defmethod filter ((hd handwritten-digit) &aux filtered)
  (setf filtered (make-array (list *image-size* *image-size*) :initial-element 0))
  (dotimes (y *image-size*)
    (dotimes (x *image-size*)
      (if (> (aref (handwritten-digit-pixels hd) x y 0) *threshold*) (setf (aref filtered x y) 0) (setf (aref filtered x y) 1))))
  (setf (handwritten-digit-pixels hd) filtered))

;; Get and store vertical slices
(defmethod vertical-slice ((hd handwritten-digit) &aux columns pixels current-pixel current-column)
  (setf pixels (handwritten-digit-pixels hd))
  (setf columns ())
  (dotimes (y *image-size*)
    (setf current-pixel (aref pixels 0 0))
    (setf current-column ())
    (dotimes (x *image-size*)
      (if (not (equal (aref pixels x y) current-pixel))
        (let ((p (aref pixels x y)))
          (setf current-pixel p)
          (setf current-column (cons p current-column)))))
    (setf current-column (cons current-pixel (reverse current-column)))
    (if (> (length current-column) 1) (setf columns (cons current-column columns))))
  (setf (handwritten-digit-vertical-slice hd) (remove-similar-neighbors (reverse columns))))

;; Get and store horizontal slices
(defmethod horizontal-slice ((hd handwritten-digit) &aux rows pixels current-pixel current-row)
  (setf pixels (handwritten-digit-pixels hd))
  (setf rows ())
  (dotimes (y *image-size*)
    (setf current-pixel (aref pixels 0 0))
    (setf current-row ())
    (dotimes (x *image-size*)
      (if (not (equal (aref pixels y x) current-pixel))
        (let ((p (aref pixels y x)))
          (setf current-pixel p)
          (setf current-row (cons p current-row)))))
    (setf current-row (cons current-pixel (reverse current-row)))
    (if (> (length current-row) 1) (setf rows (cons current-row rows))))
  (setf (handwritten-digit-horizontal-slice hd) (remove-similar-neighbors (reverse rows))))

(defmethod horizontal-slice-string ((hd handwritten-digit) &aux output)
  (setf output "")
  (loop for slice in (handwritten-digit-horizontal-slice hd) do
        (setf output (concatenate 'string output (numlist-to-string slice))))
  output)

(defmethod vertical-slice-string ((hd handwritten-digit) &aux output)
  (setf output "")
  (loop for slice in (handwritten-digit-vertical-slice hd) do
        (setf output (concatenate 'string output (numlist-to-string slice))))
  output)

;;;;
;; Edge Thinning - Zhang-Suen

;; Count neighbors
(defmethod number-of-neighbors ((pixels array) (x integer) (y integer))
  (+ 
    (aref pixels (- y 1) (- x 1))
    (aref pixels (- y 1) x)
    (aref pixels (- y 1) (+ x 1))
    (aref pixels y (- x 1))
    (aref pixels y (+ x 1))
    (aref pixels (+ y 1) (- x 1))
    (aref pixels (+ y 1) x)
    (aref pixels (+ y 1) (+ x 1))))

;; Count the number of transitions in P2,P3,P4,P5,P6,P7,P8,P9,P2
;; and a kernel which looks like:
;;
;;      P9  P2  P3
;;      P8  P1  P4
;;      P7  P6  P5
;;
(defmethod number-of-transitions ((pixels array) (x integer) (y integer) &aux circle current-value transitions)
  (setf transitions 0)
  (setf current-value 2)
  (setf circle (list 
    (aref pixels (- y 1) x) ;p2
    (aref pixels (- y 1) (+ x 1)) ;p3 
    (aref pixels y (+ x 1)) ;p4
    (aref pixels (+ y 1) (+ x 1)) ;p5
    (aref pixels (+ y 1) x) ;p6
    (aref pixels (+ y 1) (- x 1)) ;p7
    (aref pixels y (- x 1)) ;p8
    (aref pixels (- y 1) (- x 1)) ;p9
    (aref pixels (- y 1) x))) ;p2
  (loop for n in circle do
        (if (and
              (equal current-value 0)
              (equal n 1))
          (setf transitions (+ transitions 1)))
        (if (not (equal current-value n))
            (setf current-value n)))
  transitions)

;; Recursive function
(defmethod thin-edges-1 ((hd handwritten-digit) &aux pixels changed next-pixels)
  (setf changed nil)
  (setf pixels (handwritten-digit-pixels hd))
  (setf next-pixels (copy-array pixels))
  (loop for y from 1 to (- *image-size* 1) do
        (loop for x from 1 to (- *image-size* 1) do
              (if (and
                    (equal (aref pixels y x) 1)
                    (<= 2 (number-of-neighbors pixels x y))
                    (<= (number-of-neighbors pixels x y) 6)
                    (equal (number-of-transitions pixels x y) 1)
                    (or
                      ; p2 p4 p6 are 0?
                      (equal (aref pixels (- y 1) x) 0)
                      (equal (aref pixels (+ y 1) x) 0)
                      (equal (aref pixels y (+ x 1)) 0))
                    (or
                      ; p4 p6 p8 are 0?
                      (equal (aref pixels y (+ x 1)) 0)
                      (equal (aref pixels (+ y 1) x) 0)
                      (equal (aref pixels y (- x 1)) 0)))
                (let ()
                  (setf (aref next-pixels y x) 0)
                  (setf changed t)))))
  (setf (handwritten-digit-pixels hd) next-pixels)
  (setf next-pixels (copy-array pixels))
  (loop for y from 1 to (- *image-size* 1) do
        (loop for x from 1 to (- *image-size* 1) do
              (if (and
                    (equal (aref pixels y x) 1)
                    (<= 2 (number-of-neighbors pixels x y))
                    (<= (number-of-neighbors pixels x y) 6)
                    (equal (number-of-transitions pixels x y) 1)
                    (or
                      ; p2 p4 p8 are 0?
                      (equal (aref pixels (- y 1) x) 0)
                      (equal (aref pixels y (+ x 1)) 0)
                      (equal (aref pixels y (- x 1)) 0))
                    (or
                      ; p2 p6 p8 are 0?
                      (equal (aref pixels (- y 1) x) 0)
                      (equal (aref pixels (+ y 1) x) 0)
                      (equal (aref pixels y (- x 1)) 0)))
                (let ()
                  (setf (aref next-pixels y x) 0)
                  (setf changed t)))))
  (setf (handwritten-digit-pixels hd) next-pixels)
  changed)
 
;; Main function
(defmethod thin-edges ((hd handwritten-digit) &aux thinning)
  (setf thinning t)
  (loop while thinning do
        (setf thinning (thin-edges-1 hd))))

;; Count ends using different kernels
;; Note: there is probably a much more efficient way to do this, 
;;       
(defmethod count-ends ((hd handwritten-digit) &aux pixels end-count)
  (setf pixels (handwritten-digit-pixels hd))
  (setf end-count 0)
  (loop for kernel in *end-kernels* do
        (loop for y from 1 to (- *image-size* 2) do
              (loop for x from 1 to (- *image-size* 2) do
                    (if (and
                          (equal (aref pixels (- y 1) (- x 1)) (nth 0 kernel))
                          (equal (aref pixels (- y 1) x) (nth 1 kernel))
                          (equal (aref pixels (- y 1) (+ x 1)) (nth 2 kernel))
                          (equal (aref pixels y (- x 1)) (nth 3 kernel))
                          (equal (aref pixels y x) (nth 4 kernel))
                          (equal (aref pixels y (+ x 1)) (nth 5 kernel))
                          (equal (aref pixels (+ y 1) (- x 1)) (nth 6 kernel))
                          (equal (aref pixels (+ y 1) x) (nth 7 kernel))
                          (equal (aref pixels (+ y 1) (+ x 1)) (nth 8 kernel)))
                      (setf end-count (+ end-count 1))))))
  (setf (handwritten-digit-end-counts hd) end-count)
  end-count)

;;;;
;; Database 

(defclass entry ()
  ((digit
    :accessor entry-digit
    :initarg :digit)
   (horizontal-stats
     :accessor entry-horizontal-stats
     :initarg :horizontal-stats)
   (vertical-stats
     :accessor entry-vertical-stats
     :initarg :vertical-stats)
   (end-counts
     :accessor entry-end-counts
     :initarg :end-counts)
   (file-count
     :accessor entry-file-count
     :initarg :file-count)))

;; Return the index of the slice in the supplied list if it exists
(defun slice-index (slice row lst &aux ti index)
  (setf index nil)
  (setf ti 0)
  (loop for e in (nth row lst) do
        (if (equal (car e) slice)
          (setf index ti))
        (setf ti (+ ti 1)))
  index)

;; Increments a count for the slice at a given index
(defun increment-slice-count (index row lst)
  (setf (second (nth index (nth row lst))) (+ (second (nth index (nth row lst))) 1)))

;; Create a new slice within the list with a count of 1
(defun add-new-slice (slice row lst)
  (cond
    ((null lst)
     (setf lst (list (list (list slice 1)))))
    ((null (nth row lst))
     (setf lst (snoc (list (list slice 1)) lst)))
    (t
      (setf (nth row lst) (snoc (list slice 1) (nth row lst)))))
  lst)

;; Read image files in training directory and fill the database
(defun train-database (&aux dir img horizontal-stats vertical-stats end-counts row file-count)
  (dotimes (i 10)
    (format t "training the digit ~A~%" i)
    (setf file-count 0)
    ; Grab the directory, named by the number of the digit
    (setf dir (directory (make-pathname :directory `(:relative "training" ,(write-to-string i)) :name :wild :type "png")))
    (setf end-counts ())
    (setf vertical-stats ())
    (setf horizontal-stats ())
    (loop for file in dir do
          (setf img (create-handwritten-digit (namestring file)))
          ; if end count is not already in list, add it
          (if (not (member (handwritten-digit-end-counts img) end-counts))
            (setf end-counts (cons (handwritten-digit-end-counts img) end-counts)))
          ; learn vertical slicing
          (setf file-count (+ file-count 1))
          (setf row 0)
          (loop for x in (handwritten-digit-vertical-slice img) do
                (let ((si (slice-index x row vertical-stats)))
                  (if si
                    (increment-slice-count si row vertical-stats)
                    (setf vertical-stats (add-new-slice x row vertical-stats))))
                (setf row (+ row 1)))
          ; learn horizontal slicing
          (setf row 0)
          (loop for x in (handwritten-digit-horizontal-slice img) do
                (let ((si (slice-index x row horizontal-stats)))
                  (if si
                    (increment-slice-count si row horizontal-stats)
                    (setf horizontal-stats (add-new-slice x row horizontal-stats))))
                (setf row (+ row 1))))
    ; all files for digit have been read, make the entry in the database
    (setf (gethash i *database*) (make-instance 'entry :digit i :horizontal-stats horizontal-stats :vertical-stats vertical-stats :end-counts end-counts :file-count file-count))))

;; Get the best horizontal slice for this digit as a string for levenshtein distance analysis
(defun best-horizontal-slice-string (digit &aux horizontal-stats current-max-value current-max-slice output)
  (setf horizontal-stats (entry-horizontal-stats (gethash digit *database*)))
  (setf output "") 
  (loop for row in horizontal-stats do
        (setf current-max-value 0)
        (setf current-max-slice nil)
        (loop for entry in row do
              (if (and 
                    (> (second entry) current-max-value) 
                    (> (second entry) (* (entry-file-count (gethash digit *database*)) *entry-threshold*)))
                (let ()
                  (setf current-max-value (second entry))
                  (setf current-max-slice (car entry)))))
        (setf output (concatenate 'string output (numlist-to-string current-max-slice))))
  output)

;; Get the best vertical slice for this digit as a string for levenshtein distance analysis
(defun best-vertical-slice-string (digit &aux vertical-stats current-max-value current-max-slice output)
  (setf vertical-stats (entry-vertical-stats (gethash digit *database*)))
  (setf output "")
  (loop for row in vertical-stats do
        (setf current-max-value 0)
        (setf current-max-slice nil)
        (loop for entry in row do
              (if (and
                    (> (second entry) current-max-value)
                    (> (second entry) (* (entry-file-count (gethash digit *database*)) *entry-threshold*)))
                (let ()
                  (setf current-max-value (second entry))
                  (setf current-max-slice (car entry)))))
        (setf output (concatenate 'string output (numlist-to-string current-max-slice))))
  output)

;; Identify the digit
(defun identify (path &aux img best-digit best-lev)
  (setf best-digit 0)
  (setf best-lev 9999)
  (setf img (create-handwritten-digit path))
  (dotimes (i 10)
    (let ((this-lev-horizontal (levenshtein-distance (best-horizontal-slice-string i) (horizontal-slice-string img)))
          (this-lev-vertical (levenshtein-distance (best-vertical-slice-string i) (vertical-slice-string img)))
          (end-counts (entry-end-counts (gethash i *database*))))
      (if (and (< (+ this-lev-vertical this-lev-horizontal) best-lev) (member (handwritten-digit-end-counts img) end-counts))
        (let ()
          (setf best-digit i)
          (setf best-lev (+ this-lev-vertical this-lev-horizontal))))))
  best-digit)

;;;;
;; Demo helper functions

;; Generate a random handwritten digit representation
(defun random-handwritten-digit (&aux pixels)
  (setf pixels (make-array (list *image-size* *image-size*) :initial-element 0))
  (dotimes (y *image-size*)
    (dotimes (x *image-size*)
      (setf (aref pixels y x) (random 255))))
  (make-instance 'handwritten-digit :id (get-id) :pixels pixels))

;; Display the data that comes from the opticl read-png-file
(defmethod display-raw-image ((img simple-array))
  (dotimes (y (array-dimension img 0))
    (dotimes (x (array-dimension img 1))
      (format t " (")
      (dotimes (i (array-dimension img 2))
	(format t " ~A " (aref img y x i)))
      (format t ") "))
    (format t "~%")))

;; Display an image, taking only the red channel and outputing a space
;; if it is above (lighter) than 100, and a "X" if it is "darker" than 100.
(defmethod pretty-display-raw-image ((img simple-array) &aux i)
  (dotimes (y (array-dimension img 0))
    (dotimes (x (array-dimension img 1))
      (if (> (aref img y x 0) *threshold*) (setf i "X") (setf i " "))
      (format t " ~A " i))
    (format t "~%")))

;;;;
;; Demos

;; Demo 1 - Create and display 3 random digits
(defun demo-1 (&aux p)
  (dotimes (x 3)
    (setf p (random-handwritten-digit))
    (display p)))

;; Demo 2 - Load an image and display its contents
(defun demo-2 (&aux img)
  (setf img (load-digit "seven.png"))
  (format t "Printing seven.png~%~%Raw output:~%")
  (display-raw-image (handwritten-digit-pixels img))
  (format t "Pretty output:~%")
  (pretty-display-raw-image (handwritten-digit-pixels img)))

;; Demo 3 - Filter matrix values to either a 1 or a 0
(defun demo-3 (&aux img)
  (setf img (load-digit "seven.png"))
  (format t "Printing seven.png~%~%Pretty output:~%")
  (pretty-display-raw-image (handwritten-digit-pixels img))
  (format t "Filtered output:~%")
  (filter img)
  (format t "~A~%" (handwritten-digit-pixels img)))

;; Demo 4 - Vertical slicing
(defun demo-4 (&aux img)
  (setf img (load-digit "seven.png"))
  (format t "Printing seven.png~%~%Filtered output:~%")
  (filter img)
  (format t "~A~%" (handwritten-digit-pixels img))
  (vertical-slice img)
  (format t "Vertical slice:~%~A~%" (handwritten-digit-vertical-slice img)))

;; Demo 5 - Horizontal slicing
(defun demo-5 (&aux img)
  (setf img (load-digit "seven.png"))
  (format t "Printing seven.png~%~%Filtered output:~%")
  (filter img)
  (format t "~A~%" (handwritten-digit-pixels img))
  (horizontal-slice img)
  (format t "Horizontal slice:~%~A~%" (handwritten-digit-horizontal-slice img)))

;; Demo 7 - Begin training the database
(defun demo-7 ()
  (format t "Training the database...")
  (train-database)
  (format t "Querying data about the digit '7'~%")
  (format t "Vertical slices: ~A~%Horizontal slices: ~A~%" (entry-vertical-stats (gethash 7 *database*)) (entry-horizontal-stats (gethash 7 *database*)))
  (format t "Querying data about the digit '3'~%")
  (format t "Vertical slices: ~A~%Horizontal slices: ~A~%" (entry-vertical-stats (gethash 3 *database*)) (entry-horizontal-stats (gethash 3 *database*))))
