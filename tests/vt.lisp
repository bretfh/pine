(in-package :pine.test)

(def-suite* :pine.vt :in :pine)

;;;; The emulator is driven through TERM-PROCESS-OUTPUT with the byte
;;;; sequences a program actually writes, so the parser and the operations are
;;;; under test together and through the surface pine.term uses.

(defun esc (&rest parts)
  (format nil "~c~{~a~}" #\Escape parts))

(defun row-text (term y)
  (string-right-trim " " (pine.vt:term-dump-row-string term y)))

(defun visible (term)
  (string-right-trim
   (list #\Newline)
   (with-output-to-string (out)
     (dotimes (y (pine.vt:term-height term))
       (write-line (row-text term y) out)))))

(defun feed (term &rest chunks)
  (dolist (c chunks term) (pine.vt:term-process-output term c)))

(defun term-of (text &key (width 20) (height 4) (max-scrollback 10000))
  (let ((term (pine.vt:make-term :width width :height height
                                 :max-scrollback max-scrollback)))
    (pine.vt:term-process-output term text)
    term))

(test printable-text-lands-on-the-grid
  (let ((term (term-of "hello")))
    (is (string= "hello" (row-text term 0)))
    (is (= 5 (pine.vt:term-cursor-x term)))
    (is (= 0 (pine.vt:term-cursor-y term)))))

(test sgr-osc-and-clear
  (let ((term (pine.vt:make-term :width 40 :height 6)))
    (feed term
          (format nil "plain text line~%")
          (esc "[31m") "red" (esc "[0m") " normal "
          (esc "[1;4;32m") "bold-ul-green" (esc "[0m") (format nil "~%")
          (esc "]0;my-title") (string #\Bel)
          (esc "[2J") (esc "[H") (format nil "top-left after clear~%")
          (esc "[31;44m") "colored" (esc "[m") " done")
    (is (string= "my-title" (pine.vt:term-title term)))
    (is (string= (format nil "top-left after clear~%colored done") (visible term)))
    (let ((face (pine.vt:cell-face (aref (pine.vt:term-grid-row term 1) 0))))
      (is (eql 1 (pine.vt:face-fg face)))
      (is (eql 4 (pine.vt:face-bg face))))))

(test osc-split-across-chunks
  (let ((term (pine.vt:make-term :width 20 :height 4)))
    (feed term (esc "]0;half") (format nil "-and-half~c" #\Bel))
    (is (string= "half-and-half" (pine.vt:term-title term)))))

(test sgr-indexed-and-truecolor
  (let ((term (pine.vt:make-term :width 20 :height 2)))
    (feed term (esc "[38;5;200m") "a" (esc "[48;2;10;20;30m") "b")
    (let ((fg (pine.vt:cell-face (aref (pine.vt:term-grid-row term 0) 0)))
          (bg (pine.vt:cell-face (aref (pine.vt:term-grid-row term 0) 1))))
      (is (eql 200 (pine.vt:face-fg fg)))
      (is (equal '(10 20 30) (pine.vt:face-bg bg))))))

(test bright-sgr-codes-are-the-high-eight
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (feed term (esc "[91m") "x" (esc "[102m") "y")
    (is (eql 9 (pine.vt:face-fg
                (pine.vt:cell-face (aref (pine.vt:term-grid-row term 0) 0)))))
    (is (eql 10 (pine.vt:face-bg
                 (pine.vt:cell-face (aref (pine.vt:term-grid-row term 0) 1)))))))

(test render-line-reports-face-runs-at-their-columns
  (let ((term (pine.vt:make-term :width 20 :height 2)))
    (feed term "ab" (esc "[7m") "cd" (esc "[0m") "ef")
    (multiple-value-bind (chars changes) (pine.vt:term-render-line term 0)
      (is (string= "abcdef" (subseq chars 0 6)))
      (is (= 4 (length changes)))
      (is (= 2 (first (second changes))))
      (is (equal '(:inverse t) (second (second changes)))))))

(test scroll-pushes-exact-rows-to-scrollback
  (let ((term (pine.vt:make-term :width 20 :height 4 :max-scrollback 8)))
    (dotimes (i 10) (feed term (format nil "line-~d~%" i)))
    (is (string= (format nil "line-7~%line-8~%line-9") (visible term)))
    (let ((n (pine.vt::term-scrollback-size term))
          (ring (pine.vt::term-scrollback term))
          (head (pine.vt::term-scrollback-head term)))
      (is (= 7 n))
      (dotimes (i n)
        (let ((row (aref ring (mod (+ head i) (length ring)))))
          (is (string= (format nil "line-~d" i)
                       (string-trim " " (map 'string #'pine.vt:cell-char row)))))))))

(test scrollback-is-capped-and-drops-the-oldest
  (let ((term (pine.vt:make-term :width 20 :height 2 :max-scrollback 3)))
    (dotimes (i 20) (feed term (format nil "l~d~%" i)))
    (is (= 3 (pine.vt::term-scrollback-size term)))
    (let* ((ring (pine.vt::term-scrollback term))
           (head (pine.vt::term-scrollback-head term))
           (oldest (aref ring (mod head (length ring)))))
      (is (string= "l16" (string-trim " " (map 'string #'pine.vt:cell-char oldest)))))))

(test the-alternate-screen-leaves-the-main-grid-alone
  (let ((term (pine.vt:make-term :width 20 :height 3)))
    (feed term "main content")
    (feed term (esc "[?1049h"))
    (is (string= "" (visible term)))
    (feed term "alt content")
    (is (string= "alt content" (row-text term 0)))
    (feed term (esc "[?1049l"))
    (is (string= "main content" (row-text term 0)))))

(test the-alternate-screen-restores-the-cursor
  (let ((term (pine.vt:make-term :width 20 :height 3)))
    (feed term (esc "[2;5H") (esc "[?1049h") (esc "[3;9H") (esc "[?1049l"))
    (is (= 1 (pine.vt:term-cursor-y term)))
    (is (= 4 (pine.vt:term-cursor-x term)))))

(test a-scroll-region-scrolls-only-its-own-rows
  (let ((term (pine.vt:make-term :width 10 :height 5 :max-scrollback 0)))
    (feed term (format nil "a~%b~%c~%d~%e"))
    (feed term (esc "[2;4r") (esc "[4;1H") (format nil "~%x"))
    (is (string= "a" (row-text term 0)))
    (is (string= "c" (row-text term 1)))
    (is (string= "d" (row-text term 2)))
    (is (string= "x" (row-text term 3)))
    (is (string= "e" (row-text term 4)))))

(test insert-and-delete-line-inside-the-region
  (let ((term (pine.vt:make-term :width 10 :height 4 :max-scrollback 0)))
    (feed term (format nil "a~%b~%c~%d"))
    (feed term (esc "[2;1H") (esc "[L"))
    (is (equal '("a" "" "b" "c") (list (row-text term 0) (row-text term 1)
                                       (row-text term 2) (row-text term 3))))
    (feed term (esc "[2;1H") (esc "[M"))
    (is (equal '("a" "b" "c" "") (list (row-text term 0) (row-text term 1)
                                       (row-text term 2) (row-text term 3))))))

(test insert-and-delete-character-shift-the-line
  (let ((term (pine.vt:make-term :width 10 :height 1)))
    (feed term "abcdef" (esc "[1;3H") (esc "[2@"))
    (is (string= "ab  cdef" (row-text term 0)))
    (feed term (esc "[1;3H") (esc "[2P"))
    (is (string= "abcdef" (row-text term 0)))))

(test erase-in-line-takes-its-mode
  (let ((term (pine.vt:make-term :width 10 :height 1)))
    (feed term "abcdefgh" (esc "[1;4H") (esc "[0K"))
    (is (string= "abc" (row-text term 0)))
    (feed term (esc "[1;1H") "abcdefgh" (esc "[1;4H") (esc "[1K"))
    (is (string= "    efgh" (row-text term 0)))
    (feed term (esc "[2K"))
    (is (string= "" (row-text term 0)))))

(test erase-in-display-takes-its-mode
  (let ((term (pine.vt:make-term :width 6 :height 3)))
    (feed term (format nil "aaa~%bbb~%ccc") (esc "[2;2H") (esc "[0J"))
    (is (equal '("aaa" "b" "") (list (row-text term 0) (row-text term 1)
                                     (row-text term 2))))
    (feed term (esc "[1;1H") (format nil "aaa~%bbb~%ccc") (esc "[2;2H") (esc "[1J"))
    (is (equal '("" "  b" "ccc") (list (row-text term 0) (row-text term 1)
                                       (row-text term 2))))))

(test erase-in-display-3-drops-the-scrollback
  (let ((term (pine.vt:make-term :width 10 :height 2 :max-scrollback 50)))
    (dotimes (i 8) (feed term (format nil "l~d~%" i)))
    (is (plusp (pine.vt::term-scrollback-size term)))
    (feed term (esc "[3J"))
    (is (zerop (pine.vt::term-scrollback-size term)))))

(test erase-character-blanks-in-place
  (let ((term (pine.vt:make-term :width 10 :height 1)))
    (feed term "abcdef" (esc "[1;2H") (esc "[3X"))
    (is (string= "a   ef" (row-text term 0)))))

(test resize-keeps-the-overlapping-cells
  (let ((term (pine.vt:make-term :width 10 :height 3)))
    (feed term (format nil "abcdefghij~%klmnopqrst"))
    (pine.vt:term-resize term 5 2)
    (is (= 5 (pine.vt:term-width term)))
    (is (= 2 (pine.vt:term-height term)))
    (is (string= "abcde" (row-text term 0)))
    (is (string= "klmno" (row-text term 1)))))

(test resize-clamps-the-cursor
  (let ((term (pine.vt:make-term :width 20 :height 6)))
    (feed term (esc "[6;20H"))
    (pine.vt:term-resize term 8 3)
    (is (= 7 (pine.vt:term-cursor-x term)))
    (is (= 2 (pine.vt:term-cursor-y term)))))

(test dec-line-drawing-maps-the-glyphs
  (let ((term (pine.vt:make-term :width 10 :height 1)))
    (feed term (esc "(0") "qxj" (esc "(B") "q")
    (is (string= (format nil "~c~c~cq"
                         (code-char #x2500) (code-char #x2502) (code-char #x2518))
                 (row-text term 0)))))

(test a-wide-character-takes-two-cells
  (let ((term (pine.vt:make-term :width 10 :height 1))
        (wide (code-char #x4F60)))
    (feed term (format nil "~ca" wide))
    (is (= 2 (pine.vt:char-display-width wide)))
    (is (char= wide (pine.vt:cell-char (aref (pine.vt:term-grid-row term 0) 0))))
    (is (char= #\Space (pine.vt:cell-char (aref (pine.vt:term-grid-row term 0) 1))))
    (is (char= #\a (pine.vt:cell-char (aref (pine.vt:term-grid-row term 0) 2))))
    (is (= 3 (pine.vt:term-cursor-x term)))))

(test a-combining-character-takes-no-cell
  (is (= 0 (pine.vt:char-display-width (code-char #x0301))))
  (is (= 1 (pine.vt:char-display-width #\a))))

(test autowrap-moves-to-the-next-line
  (let ((term (pine.vt:make-term :width 4 :height 2)))
    (feed term "abcdef")
    (is (string= "abcd" (row-text term 0)))
    (is (string= "ef" (row-text term 1)))))

(test carriage-return-and-backspace-move-without-erasing
  (let ((term (pine.vt:make-term :width 10 :height 1)))
    (feed term "abcdef" (string #\Return) "XY")
    (is (string= "XYcdef" (row-text term 0)))
    (feed term (string #\Backspace) "Z")
    (is (string= "XZcdef" (row-text term 0)))))

(test tabs-advance-to-the-eight-column-stops
  (let ((term (pine.vt:make-term :width 40 :height 1)))
    (feed term "a" (string #\Tab) "b")
    (is (= 9 (pine.vt:term-cursor-x term)))
    (is (string= "a       b" (row-text term 0)))))

(test save-and-restore-cursor-carry-the-attributes
  (let ((term (pine.vt:make-term :width 20 :height 3)))
    (feed term (esc "[31m") (esc "7") (esc "[2;5H") (esc "[0m") (esc "8") "x")
    (is (= 0 (pine.vt:term-cursor-y term)))
    (is (eql 1 (pine.vt:face-fg
                (pine.vt:cell-face (aref (pine.vt:term-grid-row term 0) 0)))))))

(test reset-clears-the-grid-and-the-modes
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (feed term (esc "[?25l") (esc "[?1049h") "junk" (esc "c"))
    (is (string= "" (visible term)))
    (is (pine.vt:term-cursor-visible term))
    (is (= 0 (pine.vt:term-cursor-x term)))
    (is (= 0 (pine.vt:term-cursor-y term)))))

(test cursor-visibility-and-bracketed-paste-follow-their-modes
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (feed term (esc "[?25l"))
    (is-false (pine.vt:term-cursor-visible term))
    (feed term (esc "[?25h"))
    (is-true (pine.vt:term-cursor-visible term))
    (feed term (esc "[?2004h"))
    (is-true (pine.vt:term-bracketed-paste term))))

(test a-device-status-report-answers-the-cursor-position
  (let* ((answers nil)
         (term (pine.vt:make-term
                :width 20 :height 5
                :input-fn (lambda (term bytes) (declare (ignore term))
                            (push bytes answers)))))
    (feed term (esc "[3;7H") (esc "[6n"))
    (is (equal (list (esc "[3;7R")) answers))))

(test the-bell-reaches-its-hook
  (let* ((rung 0)
         (term (pine.vt:make-term :width 5 :height 1
                                  :bell-fn (lambda (term) (declare (ignore term))
                                             (incf rung)))))
    (feed term (format nil "a~cb" #\Bel))
    (is (= 1 rung))))

(test the-title-hook-sees-what-the-title-becomes
  (let* ((seen nil)
         (term (pine.vt:make-term :width 5 :height 1
                                  :title-fn (lambda (term text)
                                              (declare (ignore term))
                                              (setf seen text)))))
    (feed term (esc "]2;a title") (string #\Bel))
    (is (string= "a title" seen))
    (is (string= "a title" (pine.vt:term-title term)))))

(test color-index-to-rgb-covers-the-cube-and-the-ramp
  (is (equalp #(0 0 0) (pine.vt:color-index-to-rgb 16)))
  (is (equalp #(255 255 255) (pine.vt:color-index-to-rgb 231)))
  (is (equalp #(8 8 8) (pine.vt:color-index-to-rgb 232)))
  (is (null (pine.vt:color-index-to-rgb 256)))
  (is (null (pine.vt:color-index-to-rgb -1))))

(test key-events-become-the-sequences-a-program-expects
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (is (string= (esc "[A") (pine.vt:key-event-to-escape-sequence term '(:up))))
    (is (string= (esc "[1;5A")
                 (pine.vt:key-event-to-escape-sequence term '(:up :ctrl))))
    (is (string= (esc "[3~") (pine.vt:key-event-to-escape-sequence term '(:delete))))
    (is (string= (string #\Return)
                 (pine.vt:key-event-to-escape-sequence term '(:enter))))
    (is (string= (string #\Rubout)
                 (pine.vt:key-event-to-escape-sequence term '(:backspace))))
    (is (string= (esc "[Z")
                 (pine.vt:key-event-to-escape-sequence term '(:tab :shift))))
    (is (string= "a" (pine.vt:key-event-to-escape-sequence term #\a)))))

(test the-keypad-mode-changes-the-arrow-prefix
  (let ((term (pine.vt:make-term :width 10 :height 2)))
    (feed term (esc "[?1h"))
    (is (string= (esc "OA") (pine.vt:key-event-to-escape-sequence term '(:up))))))

(test an-escape-split-across-chunks-is-not-printed-and-still-acts
  (let ((term (pine.vt:make-term :width 20 :height 40)))
    (feed term "ab" (esc "[3"))
    (is (string= "ab" (row-text term 0)))
    (feed term "1;5H")
    (is (= 30 (pine.vt:term-cursor-y term)))
    (is (= 4 (pine.vt:term-cursor-x term)))))
