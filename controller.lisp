(defvar defined-tasks `())

;; Just a generally useful function, provided free of charge
(defun argmax(set toMaximize &optional get-val? &aux (max `NIL))
    "Using 'set', find the argument that maximizes the 'toMaximize' expression; if get-val is
    specified, find the argument and its value."
    (dolist (cur set (or (and get-val? max) (first max)))
        (let ((val (funcall toMaximize cur)))
            (if (or (not max) (> val (second max)))
                (setf max `(,cur ,val))))))

;; This function adds the task to the database, also adding it to whatever conditions are necessary
;; to process the item effectively.
(defun add(name due-date &rest conditions)
    (setf defined-tasks (cons `(,name ,due-date ,conditions) defined-tasks)))

;; Selects the newest task from a list of tasks
(defun select-newest(tasks)
    (argmax tasks #'(lambda(x)
        (- (+ (* 1000 (first (second x))) (* 100 (second (second x))) (third (second x)))))))

(defun get-oldest-by-name(name &aux ties)
 (select-newest (dolist (cur-task defined-tasks ties)
        (if (equal (first cur-task) name)
            (setf ties (cons cur-task ties))))))

;; Removes the task (as if it never existed)
(defun remove-task(name)
    (setf defined-tasks (remove (get-oldest-by-name name) defined-tasks)))

;; Organizd and displays every task in a given list of tasks
(defun show-tasks (tasks)
    (mapcar #'(lambda(task)
        (format t "~A: ~A-~A-~A~&" (first task) (first (second task)) (second (second task)) (third (second task)))
        ) tasks))

;; Gets all tasks associated with a certain tag
(defun get-tasks-of-tag(tag &aux tasks)
    (dolist (cur-task defined-tasks tasks)
        (if (find tag (third cur-task))
            (setf tasks (cons cur-task tasks)))))

;; Gets all tasks that pass the function for a rule
(defun get-tasks-of-function(rule &aux tasks)
    (mapcar #'(lambda(task)
        (if (funcall rule task)
            (setf tasks (cons task tasks)))) defined-tasks)
    tasks)

;; Returns all tasks associated with a condition, whether that condition be a rule or a tag. Note
;; that, in the case of being both a rule and a tag, only the rule will be detected.
(defun get-tasks-with-condition(condition &aux tasks)
    ;; ((fboundp something) = T) => (something = function)
    (if (fboundp condition)
        (get-tasks-of-function condition)
        (get-tasks-of-tag condition)))

;; Returns the seconds corresponding to the start of day "date." Because due dates are not allowed
;; to contain seconds, this scheme will not cause problems.
(defun date-to-time(date)
    (encode-universal-time 0 0 0 (third date) (second date) (first date) (nth-value 8 (get-decoded-time))))

;; Returns the date of the time (in seconds) in the format `(year month day). If no argument is
;; specified, returns today's date.
(defun time-to-date(&optional (seconds (get-universal-time)))
    (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (decode-universal-time seconds) (declare (ignore sec min hr dow dst-p tz))
    (list yr mon day)))

;; Returns the current day of the week (in numerical form)
(defun day-of-week()
    (nth-value 6 (get-decoded-time)))

;; Views the union of all tasks associated with the specified rules; if no rules are specified, then
;; views all incomplete tasks.
(defun view(&rest rules)
    (format t "~%")
    (show-tasks (cond
        ((eq (length rules) 0) defined-tasks)
        ((eq (length rules) 1) (get-tasks-with-condition (first rules)))
        ((eq (length rules) 2) (union (get-tasks-with-condition (first rules)) (get-tasks-with-condition (second rules))))
        ((>  (length rules) 2) (let ((tasks (get-tasks-with-condition (first rules))))
            (dolist (cur (rest rules) tasks)
                (setf tasks (union tasks (get-tasks-with-condition cur))))))))
    (format t "~%")
    (values))

;; Moves task to "completed_tasks.dat" and removes it from the currently defined task
(defun mark-completed(name)
    (let ((file (open "completed_tasks.dat" :direction :output :if-does-not-exist :create :if-exists :append)))
        (format file "~S~%" (get-oldest-by-name name))
        (close file)
        (remove-task name)))

;; Writes files to the "user_tasks.dat" file; if not present, the file is created. If preesnt,
;; the contents of the file are overwritten.
(defun flush()
    (let ((file (open "user_tasks.dat" :direction :output :if-does-not-exist :create :if-exists :supersede)))
        (dolist (task defined-tasks)
            (format file "~S~%" task))
        (close file)))

;; Loads tasks from the "user_tasks.dat" file; if not present, the file is created.
(defun load-tasks-from-file()
    (let ((in (open "user_tasks.dat" :if-does-not-exist :create)))
        (when in
            (loop for line = (read-line in nil)
                while line do (eval (read-from-string (format `NIL "(setf defined-tasks (union `(~A) defined-tasks :test 'equal))" line))))
            (close in))))

;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
;;                                Generic Defined Tags and Rules                                  ;;
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;

(defun today(task)
    (<= (date-to-time (second task)) (get-universal-time)))

(defun tomorrow(task)
    (equal (second task) (time-to-date (+ (get-universal-time) 86400))))

(defun week(task)
    (<= (date-to-time (second task)) (+ (get-universal-time) 518400)))