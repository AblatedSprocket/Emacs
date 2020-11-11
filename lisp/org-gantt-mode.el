;;; org-gantt-mode.el --- Generate GANTT charts from an org-mode subtree

;; Author: Jouke Hijlkema hylkema@free.fr
;; URL: https://gitlab.com/joukeHijlkema/org-gantt
;; Version: 0.1
;; Packages-Required: ((emacs "26.1") (svg "1.0") (ts "20191010.210"))
;; Keywords: org-mode gantt svg

;; Left to do:
;; DONE: allow for an offset in the before and after lists (After T1.2+2w)
;; - allow for a fixed start or end date
;; - Allow for efforts in days and hence a day bar in the grid header
;; - Alow for selctive output (period, max depth)

(require 'svg)
(require 'ts)
(defvar og-Cols '((strokeColor  . "black")
                  (bgTitleBlock . "lightgray")
                  (bgGridHead   . "gray")
                  (bgTaskEven   . "lightgray")
                  (bgTaskOdd    . "gray")
                  (bgTaskLevel1 . "darkgreen")
                  (taskDone     . "red")
                  (fillTaskLevel1 . "black")
                  (bgTask       . "blue")
                  (bgKP         . "purple")
                  (stLink       . "red")
		  (bgBase       . "white")
                  )
  "default gantt colors")
(defvar og-Dims '((rowH . 21)
                  (headRows . 3))
  "default dimensions for gantt")
(defvar t-Offset 5
  "Vertical offset of text.")

(defvar og-Text '((fontSize . 20)
                  (fontFamily . "sans")
                  (dateFormat . "%d/%m/%Y"))
  "default font stuff for gantt")

(cl-defstruct myTask
  Id
  Kids
  Description
  Type
  Parent
  Before
  After
  Start
  End
  Level
  Duration
  startOffset
  endOffset
  Done
  X1
  X2
  Y
  )

;;|--------------------------------------------------------------
;;|Description : Parse the subtree and create a list of tasks
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-01-2020 14:01:49
;;|--------------------------------------------------------------
(defun og-parse (data &optional edna)
  "Parse the subtree and create a list of tasks"
  (interactive "P")
  (make-local-variable 'og-project)
  (make-local-variable 'og-header)
  (make-local-variable 'og-title)
  (make-local-variable 'og-start)
  (make-local-variable 'og-end)
  (make-local-variable 'og-changed)
  (make-local-variable 'og-counter)
  (setq og-header (nth 0 data))
  (setq og-title (nth (seq-position og-header "item") (nth 1 data)))
  (setq og-start (ts-parse (nth (seq-position og-header "start") (nth 1 data))))
  
  (setq og-end og-start)
  
  (setq og-project (seq-map 'og-parse-task (seq-subseq data 2)))
  (seq-do 'og-fix-BA og-project)
  (seq-do 'og-fix-hierarchy og-project)
  (setq og-changed t)
  (setq og-counter 0)
  (while (and og-changed (< og-counter 10))
    (setq og-changed nil)
    (seq-do 'og-calculate-times og-project)
    (cl-incf og-counter)
    (message "og-changed = %s, og-counter = %s" og-changed og-counter)
    )
  (if (> og-counter 9) (error "Could not calculate start/end dates. Probably a circular definition"))
  (seq-do (lambda (elt) (if (ts> (myTask-End elt) og-end) (setf og-end (myTask-End elt)))) og-project)
  )

;;|--------------------------------------------------------------
;;|Description : parse a line from data into a task object
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-11-2020 14:11:07
;;|--------------------------------------------------------------
(defun og-parse-task (row)
  "parse a line from data into a task object"
  (interactive "P")
  (let* ((Type (if (eq (nth (seq-position og-header "type") row) "")
                  "Task"
                 (nth (seq-position og-header "type") row)))
         (Id     (nth (seq-position og-header "task_id") row))
         (Level (if (string= Type "KP") -1
                  (- (length (split-string Id "\\.")) 1)))
         (Start (if (eq (nth (seq-position og-header "start") row) "")
                    og-start
                  (ts-parse (nth (seq-position og-header "start") row))))
         (End (if (eq (nth (seq-position og-header "end") row) "")
                    Start
                (ts-parse (nth (seq-position og-header "end") row))))
         (Duration (cond ((string= Type "KP") "0d")
                         (t (nth (seq-position og-header "eff") row))))
         (Done (string-to-number (nth (seq-position og-header "done") row)))
         )
    (message "Done = %s" Done)
    (make-myTask
     :Id          Id
     :Description (nth (seq-position og-header "item") row)
     :Type        Type
     :Before      (split-string (nth (seq-position og-header "before") row) ",")
     :After       (split-string (nth (seq-position og-header "after") row) ",")
     :Start       Start
     :End         End
     :Level       Level
     :Kids        (list)
     :Duration    Duration
     :Done        (if (> Done 1) (/ Done 100.0) Done)
     )
    )
  )
;;|--------------------------------------------------------------
;;|Description : fix the before and after sets
;;|NOTE : this is when using befor/after properties
;;|-
;;|Author : jouke hylkema
;;|date   : 18-12-2020 15:12:08
;;|--------------------------------------------------------------
(defun og-fix-BA (task)
  "fix the before and after sets"
  (interactive "P")
  ;; (unless (equal '("") (myTask-After task))
  ;; (setf (myTask-After task) (seq-map 'og-get-task-from-id (myTask-After task))))
  (let ((After (list))
        (AOffset (list))
        (Before (list))
        (BOffset (list)))
    (cl-loop for A
             in (myTask-After task)
             do (let* ((target (og-get-task-from-id (car (split-string A "+"))))
                       (offset (cadr (split-string A "+"))))
                  (if target (push target After))
                  (if offset (push offset AOffset) (push "0" AOffset))
                  )
             )
    (cl-loop for B
             in (myTask-Before task)
             do (let* ((target (og-get-task-from-id (car (split-string B "+"))))
                       (offset (cdr (split-string B "+"))))
                  (if target (push target Before))
                  (if offset (push offset BOffset))
                  )
             )
    (setf (myTask-After task) After)
    (setf (myTask-Before task) Before)
    (setf (myTask-startOffset task) AOffset)
    (setf (myTask-endOffset task) BOffset)
    )
  )
;;|--------------------------------------------------------------
;;|Description : fix before after sets when using edna formats
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 03-14-2020 18:14:31
;;|--------------------------------------------------------------
(defun og-fix-BA-edna (task)
  "fix before after sets when using edna formats"
  (interactive "P")
  (let ((After (list))
        (AOffset (list))
        (Before (list))
        (BOffset (list)))
    (cl-loop for A
             in (myTask-After task)
             do (let* ((target (og-get-task-from-id (car (split-string A "+"))))
                       (offset (cadr (split-string A "+"))))
                  (if target (push target After))
                  (if offset (push offset AOffset) (push "0" AOffset))
                  )
             )
    (cl-loop for B
             in (myTask-Before task)
             do (let* ((target (og-get-task-from-id (car (split-string B "+"))))
                       (offset (cdr (split-string B "+"))))
                  (if target (push target Before))
                  (if offset (push offset BOffset))
                  )
             )
    (setf (myTask-After task) After)
    (setf (myTask-Before task) Before)
    (setf (myTask-startOffset task) AOffset)
    (setf (myTask-endOffset task) BOffset)
    )
  
  )
;;|--------------------------------------------------------------
;;|Description : fix the hierarchy between tasks
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-05-2020 17:05:21
;;|--------------------------------------------------------------
(defun og-fix-hierarchy (task)
  "fix the hierarchy between tasks"
  (interactive "P")
  (when (> (myTask-Level task) 0)
    (let* ((parentId (string-join (seq-subseq (split-string (myTask-Id task) "\\.") 0 -1) "."))
           (parent (og-get-task-from-id parentId))
           (kids (myTask-Kids parent)))
      (push task kids)
      (setf (myTask-Parent task) parent)
      (push task (myTask-Kids parent))      
    )
    )
  )
;;|--------------------------------------------------------------
;;|Description : calculate start and end times
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 19-16-2020 09:16:32
;;|--------------------------------------------------------------
(defun og-calculate-times (task)
  "calculate start and end times"
  (interactive "P")
  (cl-loop for A being the elements of (myTask-After task)
           using (index i)
           do (unless (eq A "")
                (let* ((Of (seq-elt (myTask-startOffset task) i))
                       (S1 (myTask-Start task))
                       (E2 (og-do-dateOffset (myTask-End A) Of))
                       )
                  (when (and E2 (ts< S1 E2))
                    (setf (myTask-Start task) E2)
                    (setq og-changed t)
                    ;; (message "updated start of %s to end of %s" (myTask-Id task) (myTask-Id A))
                    )
                  )
                )
           )
  
  (when (and (myTask-Parent task) (ts> (myTask-Start (myTask-Parent task)) (myTask-Start task)))
    (setf (myTask-Start task) (myTask-Start (myTask-Parent task)))
    (setq og-changed t)
    ;; (message "updated start of %s to start of parent %s" (myTask-Id task) (myTask-Id (myTask-Parent task)))
    )

  ;; (message "Duration %s = %s" (myTask-Id task) (myTask-Duration task))
  (if (> (length (myTask-Duration task)) 0)
      (setf (myTask-End task) (og-calc-end task))
    (seq-do (lambda (K)
              ;; (message "check end of %s against end of %s" (myTask-Id task) (myTask-Id K))
              (when (ts> (myTask-End K) (myTask-End task))
                (setf (myTask-End task) (myTask-End K))
                (setq og-changed t)
                ;; (message "updated end of %s to end of %s" (myTask-Id task) (myTask-Id K))
                )
              )
            (myTask-Kids task))
    )
  )
;;|--------------------------------------------------------------
;;|Description : ofsett a timestamp by a given amount (d,w,m,y)
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 25-20-2020 18:20:01
;;|--------------------------------------------------------------
(defun og-do-dateOffset (date offset)
  "ofsett a timestamp by a given amount (d,w,m,y)"
  (interactive "P")
  (cond ((cl-search "y" offset) (ts-adjust 'year (string-to-number offset)  date))
        ((cl-search "m" offset) (ts-adjust 'month (string-to-number offset) date))
        ((cl-search "w" offset) (ts-adjust 'day  (* 7 (string-to-number offset)) date))
        ((cl-search "d" offset) (ts-adjust 'day  (string-to-number offset) date))
        (t date)
        )
  )
;;|--------------------------------------------------------------
;;|Description : get a Task from an ID
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-24-2020 16:24:23
;;|--------------------------------------------------------------
(defun og-get-task-from-id (id)
  "get a Task from an ID"
  (interactive "P")
  ;; (message "looking for %s" id)
  (car (seq-filter (lambda (B) (string= id (myTask-Id B))) og-project)))
;;|--------------------------------------------------------------
;;|Description : Print a task
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-14-2020 15:14:55
;;|--------------------------------------------------------------
(defun og-print-task (task)
  "Print a task"
  (interactive "P")
  (message "=== Task %s ===" (myTask-Id task))
  (message "| Type        : %s" (myTask-Type task))
  (message "| Level       : %s" (myTask-Level task))
  (message "| Description : %s" (myTask-Description task))
  (message "| Duration    : %s" (myTask-Duration task))
  (message "| Done        : %s%%" (if (myTask-Done task) (* 100 (myTask-Done task)) 0))
  (message "| Before      : %s" (seq-map (lambda (c) (if (eq c "") "-" (myTask-Id c))) (myTask-Before task)))
  (message "| After       : %s" (seq-map (lambda (c) (if (eq c "") "-" (myTask-Id c))) (myTask-After task)))
  (message "| Start off.  : %s" (seq-map (lambda (c) (if (eq c "") "-" c)) (myTask-startOffset task)))
  (message "| End off.    : %s" (seq-map (lambda (c) (if (eq c "") "-" c)) (myTask-endOffset task)))
  (message "| Parent      : %s" (if (myTask-Parent task) (myTask-Id (myTask-Parent task)) ) "-")
  (message "| Kids        : %s" (seq-map (lambda (c) (if (eq c "") "-" (myTask-Id c))) (myTask-Kids task)))
  (message "| Start       : %s" (ts-format (car org-time-stamp-formats) (myTask-Start task)))
  (message "| End         : %s" (ts-format (car org-time-stamp-formats) (myTask-End task)))
  (message "===============")
  )
;;|--------------------------------------------------------------
;;|Description : create a Gantt chart from an org subtree
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-56-2020 13:56:06
;;|--------------------------------------------------------------
(defun og-makeGantt (data scale path W P)
  "create a Gantt chart from an org subtree"
  (interactive "P")
  (let* ((H (* (alist-get 'rowH og-Dims) (+ (length og-project) (alist-get 'headRows og-Dims))))
         (W1 (* P W))
         (W2 (- W W1))
         (total (ts-diff og-end og-start))
         (svg (svg-create W H
                          :stroke-width 1
                          :fill "none"
                          :stroke-color (alist-get 'strokeColor og-Cols)
                          :font-size  (alist-get 'fontSize og-Text)
                          :dominant-baseline "middle"
                          :font-family  (alist-get 'fontFamily og-Text)))
         )
    (og-background svg W H)
    (og-title svg W1 (* (alist-get 'rowH og-Dims) (alist-get 'headRows og-Dims)))
    (og-gridHead svg W1 W2 (* (alist-get 'rowH og-Dims) (alist-get 'headRows og-Dims)) scale total)
    (seq-map-indexed (lambda (task row) (og-draw-task svg task row W1 W2 total)) og-project)
    (seq-do (lambda (task) (og-draw-links svg task)) og-project)
    (with-temp-file path
      (set-buffer-multibyte nil)
      (svg-print svg)
      )  
    )
  )
(defun og-background (svg W H)
  "Create a background block for a gantt SVG with width W and height H."
  (interactive "P")
  (svg-rectangle svg 0 0 W H :fill-color (alist-get 'bgBase og-Cols)))
;;|--------------------------------------------------------------
;;|Description : create a title block for a gantt
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 18-59-2020 13:59:06
;;|--------------------------------------------------------------
(defun og-title (svg W H)
  "create a title block for a gantt"
  (interactive "P")
  (svg-rectangle svg 0 0 W H :fill-color (alist-get 'bgTitleBlock og-Cols))
  (let* ((X 10)
         (H (alist-get 'rowH og-Dims))
         (Y1 (+ (* 0.5 H) t-Offset))
         (Y2 (+ Y1 H))
         (Y3 (+ Y2 H))
         (S  (format " - Start: %s" (ts-format (alist-get 'dateFormat og-Text) og-start)))
         (E  (format " - End: %s" (ts-format (alist-get 'dateFormat og-Text) og-end)))
         )
    (svg-text svg og-title :x X :y Y1  :fill "black")
    (svg-text svg S :x X :y Y2 :fill "black")
    (svg-text svg E :x X :y Y3 :fill "black")
    )
  )
;;|--------------------------------------------------------------
;;|Description : draw the gridhead as a function of the scale
;;|NOTE : 
;;|- S = 0 -> Y
;;|- S = 1 -> Y,M
;;|- S = 2 -> Y,M,W
;;|- S = 3 -> M,W,D
;;|Author : jouke hylkema
;;|date   : 23-00-2020 19:00:38
;;|--------------------------------------------------------------
(defun og-gridHead (svg X W H S total)
  "draw the gridhead as a function of the scale"
  (interactive "P")
  (svg-rectangle svg X 0 W H :fill-color (alist-get 'bgGridHead og-Cols))
  (setq Y (alist-get 'rowH og-Dims))
  ;; Years
  (when (< S 3)
    (cl-loop for (X1 p1 p2) = (list X og-start (ts-apply :month 1 :day 1 (ts-adjust 'year 1 og-start)))
             then (og-draw-gridHead-year svg Y X1 W p1 p2 total (eq S 0))
             when (ts>= p1 og-end)
             return (svg-line svg X Y (+ X W) Y))
    (cl-incf Y (alist-get 'rowH og-Dims)))
  ;; Months
  (when (> S 0)
    (cl-loop for (X1 p1 p2) = (list X og-start (ts-apply :day 1 (ts-adjust 'month 1 og-start)))
           then (og-draw-gridHead-month svg Y X1 W p1 p2 total (eq S 1))
           when (ts>= p1 og-end)
           return (svg-line svg X Y (+ X W) Y)
           )
    (cl-incf Y (alist-get 'rowH og-Dims)))
  ;; Weeks
  (when (> S 1)
    (cl-loop for (X1 p1 p2) = (list X og-start (ts-apply :dow 1 (ts-adjust 'day 7 og-start)))
           then (og-draw-gridHead-week svg Y X1 W p1 p2 total (eq S 2))
           when (ts>= p1 og-end)
           return (svg-line svg X Y (+ X W) Y)
           )
    
    (cl-incf Y (alist-get 'rowH og-Dims)))
  ;; Days
  (when (> S 1)
    (cl-loop for (X1 p1 p2) = (list X og-start (ts-apply :dom 1 (ts-adjust 'day 1 og-start)))
           then (og-draw-gridHead-day svg Y X1 W p1 p2 total (eq S 3))
           when (eq p1 og-end)
           return (svg-line svg X Y (+ X W) Y)
    )
    (cl-incf Y (alist-get 'rowH og-Dims)))
  
 )
;;|--------------------------------------------------------------
;;|Description : recursively draw a gridHead year
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-07-2020 09:07:43
;;|--------------------------------------------------------------
(defun og-draw-gridHead-year (svg Y X1 W p1 p2 total long)
  "draw a gridHead year"
  (interactive "P")
  (let* ((name (ts-Y p1))
         (fs   (alist-get 'fontSize og-Text))
         (p2   (if (ts< og-end p2) og-end p2))
         (dd   (/ (ts-diff p2 p1) total))
         (X2   (+ X1 (* dd W)))
         (Xt   (* 0.5 (+ X1 X2)))
         (Y1   (- Y (alist-get 'rowH og-Dims)))
         (Y2   (if long (* (alist-get 'rowH og-Dims)
                           (+ (length og-project) (alist-get 'headRows og-Dims)))
                 Y))
         (Yt   (+ Y1 (* 0.5 (alist-get 'rowH og-Dims)) t-Offset))
         )
    (message "%s->%s (%s)" (ts-Y p1) (ts-Y p2) dd)
    (svg-line svg X1 Y1 X1 Y2)
    (svg-text svg (format "%s" name) :x Xt :y Yt
              :fill "black"
              :font-size fs :text-anchor "middle")
    (list X2 p2 (ts-adjust 'year 1 p2))
    )
  )
;;|--------------------------------------------------------------
;;|Description : recursively draw a gridHead month
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-07-2020 09:07:43
;;|--------------------------------------------------------------
(defun og-draw-gridHead-month (svg Y X1 W p1 p2 total long)
  "draw a gridHead month"
  (interactive "P")
  (let* ((name (ts-moy p1))
         (fs   (* 0.8 (alist-get 'fontSize og-Text)))
         (p2   (if (ts< og-end p2) og-end p2))
         (dd   (/ (ts-diff p2 p1) total))
         (X2   (+ X1 (* dd W)))
         (Xt   (* 0.5 (+ X1 X2)))
         (Y1   (- Y (alist-get 'rowH og-Dims)))
         (Y2   (if long (* (alist-get 'rowH og-Dims)
                           (+ (length og-project) (alist-get 'headRows og-Dims)))
                 Y))
         (Yt   (+ Y1 (* 0.5 (alist-get 'rowH og-Dims))))
         )
    (svg-line svg X1 Y1 X1 Y2)
    (svg-text svg (format "%s" name) :x Xt :y (+ Yt t-Offset)
              :fill "black"
              :font-size fs :text-anchor "middle")
    (list X2 p2 (ts-adjust 'month 1 p2))
    )
  )
;;|--------------------------------------------------------------
;;|Description : recursively draw a gridHead week
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-07-2020 09:07:43
;;|--------------------------------------------------------------
(defun og-draw-gridHead-week (svg Y X1 W p1 p2 total long)
  "draw a gridHead week"
  (interactive "P")
  (let* ((name (ts-woy p1))
         (fs   (* 0.6 (alist-get 'fontSize og-Text)))
         (p2   (if (ts< og-end p2) og-end p2))
         (dd   (/ (ts-diff p2 p1) total))
         (X2   (+ X1 (* dd W)))
         (Xt   (* 0.5 (+ X1 X2)))
         (Y1   (- Y (alist-get 'rowH og-Dims)))
         (Y2   (if long (* (alist-get 'rowH og-Dims)
                           (+ (length og-project) (alist-get 'headRows og-Dims)))
                 Y))
         (Yt   (+ Y1 (* 0.5 (alist-get 'rowH og-Dims))))
         )
    (svg-line svg X1 Y1 X1 Y2)
    (svg-text svg (format "%s" name) :x Xt :y (+ Yt t-Offset)
              :fill "black"
              :font-size fs :text-anchor "middle")
    (list X2 p2 (ts-adjust 'day 7 p2))
    )
  )
;;|--------------------------------------------------------------
;;|Description : recursively draw a gridHead day
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-07-2020 09:07:43
;;|--------------------------------------------------------------
(defun og-draw-gridHead-day (svg Y X1 W p1 p2 total long)
  "draw a gridHead week"
  (interactive "P")
  (let* ((name (ts-dom p1))
         (fs   (* 0.6 (alist-get 'fontSize og-Text)))
         (p2   (if (ts< og-end p2) og-end p2))
         (dd   (/ (ts-diff p2 p1) total))
         (X2   (+ X1 (* dd W)))
         (Xt   (* 0.5 (+ X1 X2)))
         (Y1   (- Y (alist-get 'rowH og-Dims)))
         (Y2   (if long (* (alist-get 'rowH og-Dims)
                           (+ (length og-project) (alist-get 'headRows og-Dims)))
                 Y))
         (Yt   (+ Y1 (* 0.5 (alist-get 'rowH og-Dims))))
         )
    (svg-line svg X1 Y1 X1 Y2)
    (svg-text svg (format "%s" name) :x Xt :y Yt
              :fill "black"
              :font-size fs :text-anchor "middle")
    (list X2 p2 (ts-adjust 'day 1 p2))
    )
  )
;;|--------------------------------------------------------------
;;|Description : Draw a task
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-57-2020 10:57:52
;;|--------------------------------------------------------------
(defun og-draw-task (svg task row W1 W2 total)
  "Draw a task"
  (interactive "P")
  (let* ((col (cond ((eq 0 (myTask-Level task)) (alist-get 'bgTaskLevel1 og-Cols))
                    ((eq 0 (% row 2))           (alist-get 'bgTaskEven og-Cols))
                    (t                          (alist-get 'bgTaskOdd og-Cols))))
         (H   (alist-get 'rowH og-Dims))
         (H2  (* 0.5 H))
         (Y1  (* (+ (alist-get 'headRows og-Dims) row) H))
         (Y2  (+ Y1 H))
         (Y   (* 0.5 (+ Y1 Y2)))
         (fs  (* 0.6 (alist-get 'fontSize og-Text)))
         (dd1 (/ (ts-diff (myTask-Start task) og-start) total))
         (dd2 (/ (ts-diff (myTask-End task) og-start) total))
         (X1  (* (myTask-Done task) W1))
         (X2  (+ W1 (* dd1 W2)))
         (X4  (+ W1 (* dd2 W2)))
         (X3  (+ X2 (* (myTask-Done task) (- X4 X2))))
         (X5  (+ W1 W2))
         )
    (svg-line svg 0 Y X5 Y :stroke-color col                           :stroke-width H :opacity "0.4")
    (svg-line svg 0 Y X1 Y :stroke-color (alist-get 'taskDone og-Cols) :stroke-width H :opacity "0.3")
    (svg-line svg W1 Y1 W1 Y2)
    (setf (myTask-X1 task) X2)
    (setf (myTask-X2 task) X4)
    (setf (myTask-Y  task) Y)
    
    (svg-text svg (format "%s" (myTask-Id task) )         :x 2  :y (+ Y t-Offset) :fill "black" :font-size fs)
    (svg-text svg (format "%s" (myTask-Description task)) :x 50 :y (+ Y t-Offset) :fill "black" :font-size fs)
    (cond
     ;; Key points centered around X2
     ((string= (myTask-Type task) "KP")
      (let* ((x1 (- X2 H2))
             (x2 X2)
             (x3 (+ X2 H2))
             (y1 Y1)
             (y2 (+ Y1 H2))
             (y3 Y2))
        (svg-polygon svg `((,x1 . ,y2)
                           (,x2 . ,y1)
                           (,x3 . ,y2)
                           (,x2 . ,y3)) :fill-color (alist-get 'bgKP og-Cols))
        )
      )
     ;; Regular tasks
     ((> (myTask-Level task) 0)
      (svg-line svg X2 Y X4 Y :stroke-color (alist-get 'bgTask og-Cols)   :stroke-width H)
      (svg-line svg X2 Y X3 Y :stroke-color (alist-get 'taskDone og-Cols) :stroke-width 5)
      )
     ;; Task level 0, a hat like polygon
     ((eq (myTask-Level task) 0)
      (let* ((x1 X2)
             (x2 (+ X2 H))
             (x3 (- X4 H))
             (x4 X4)
             (y1 Y1)
             (y2 Y)
             (y3 Y2))
        (svg-polygon svg `((,x1 . ,y1)
                           (,x4 . ,y1)
                           (,x4 . ,y3)
                           (,x3 . ,y2)
                           (,x2 . ,y2)
                           (,x1 . ,y3))  :fill-color (alist-get 'fillTaskLevel1 og-Cols))
        )
      )
     )
    )
  )
;;|--------------------------------------------------------------
;;|Description : draw the links between tasks
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-29-2020 14:29:50
;;|--------------------------------------------------------------
(defun og-draw-links (svg task)
  "draw the links between tasks"
  (interactive "P")
  ;; (u (myTask-Before task) (seq-do (lambda (B) (og-draw-link svg task B)) (myTask-Before task)))
  (unless (eq 0 (length (myTask-After task)))
    (message "length : %s" (length (myTask-After task)))
    ;; (og-print-task task)
    (seq-do (lambda (A) (og-draw-link svg A task)) (myTask-After task)))
  )
;;|--------------------------------------------------------------
;;|Description : draw the actual link between a and b
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 20-33-2020 14:33:28
;;|--------------------------------------------------------------
(defun og-draw-link (svg A B)
  "draw the actual link between a and b"
  (interactive "P")
  ;; (message "A: %s" (type-of A))
  ;; (message "B: %s" (type-of B))
  (unless (or (eq (type-of A) 'string) (eq (type-of B) 'string))
    (let* ((X1 (myTask-X2 A))
           (X2 (myTask-X1 B))
           (Y1 (myTask-Y A))
           (Y2 (myTask-Y B))
           (d 5)
           (c (alist-get 'stLink og-Cols))
           )
      (svg-polyline svg `((,X1 . ,Y1) (,X1 . ,Y2) (,X2 . ,Y2)) :stroke-width 2 :stroke-color c)
      (svg-polygon svg `((,X2 . ,Y2) (,X2 . ,(+ Y2 d)) (,(+ X2 d) . ,Y2) (,X2 . ,(- Y2 d))) :fill-color c)
      )
    )
  )
;;|--------------------------------------------------------------
;;|Description : calculate date n units from start
;;|NOTE : n can be in days, weeks or months
;;|-
;;|Author : jouke hylkema
;;|date   : 19-14-2020 10:14:41
;;|--------------------------------------------------------------
(defun og-calc-end (task)
  "calculate date n units from start"
  (interactive "P")
  (let* ((start (myTask-Start task))
         (n (myTask-Duration task))
         (mode (cond ((cl-search "m" n) "M")
                     ((cl-search "w" n) "W")
                     ((cl-search "d" n) "D")
                     ((string= n "") "N")
                     (t "D")))
        )
    (cond ((string= mode "M")
           (setf (myTask-End task)
                 (og-calc-end-from-start start 'month (string-to-number (substring n 0 -1)) )))
          ((string= mode "W")
           (setf (myTask-End task)
                 (og-calc-end-from-start start 'day (* 7 (string-to-number (substring n 0 -1))) )))
          ((string= mode "D")
           (setf (myTask-End task)
                 (og-calc-end-from-start start 'day (string-to-number (substring n 0 -1)) )))
          )
    )
  )
;;|--------------------------------------------------------------
;;|Description : calculate the end date based on start and duration
;;|NOTE : duration can be in months, weeks or days
;;|-
;;|Author : jouke hylkema
;;|date   : 19-50-2020 11:50:53
;;|--------------------------------------------------------------
(defun og-calc-end-from-start (start mode duration)
  "calculate the end date based on start and duration"
  (interactive "P")
  (ts-adjust mode duration start)
  )
;;|--------------------------------------------------------------
;;|Description : Print the project
;;|NOTE : 
;;|-
;;|Author : jouke hylkema
;;|date   : 03-36-2020 18:36:05
;;|--------------------------------------------------------------
(defun og-print-project ()
  "Print the project"
  (interactive "P")
  (message "=======================")
  (message "Project %s" og-title) 
  (message "start: %s" (ts-format (alist-get 'dateFormat og-Text) og-start))
  (message "end  : %s" (ts-format (alist-get 'dateFormat og-Text) og-end))
  (message "=======================")
  (seq-do 'og-print-task og-project)
  )

(define-minor-mode org-gantt-mode
  "Create SVG gantt charts from an org subtree"
  :lighter " Gantt")

(provide 'org-gantt-mode)
