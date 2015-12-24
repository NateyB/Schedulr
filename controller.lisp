(defvar defined-rules `())
(defvar defined-tasks `())

;; Just a generally useful function, provided to you free of charge
(defun argmax(set toMaximize &optional get-val? &aux (max `NIL))
    "Using 'set', find the argument that maximizes the 'toMaximize' expression; if get-val is
    specified, find the argument and its value."
    (dolist (cur set (or (and get-val? max) (first max)))
        (let ((val (funcall toMaximize cur)))
            (if (or (not max) (> val (second max)))
                (setf max `(,cur ,val))))))

;; This function adds the task to the database, also adding it to whatever rules are necesasry
;; to process the item effectively.
(defun add(name due-date &rest rules)
    (setf defined-rules (union defined-rules rules))
    (setf defined-tasks (cons `(,name ,due-date ,rules) defined-tasks)))

(defun select-newest(tasks)
    (argmax tasks #'(lambda(x)
        (- (+ (* 1000 (first (second x))) (* 100 (second (second x))) (third (second x)))))))

(defun remove-task(name &aux ties)
    (setf defined-tasks (remove (select-newest (dolist (cur-task defined-tasks ties)
        (if (equal (first cur-task) name)
            (setf ties (cons cur-task ties))))) defined-tasks)))

(defun view-tasks (tasks)
    (mapcar #'(lambda(task)
        (format t "~A: ~A-~A-~A~&" (first task) (first (second task)) (second (second task)) (third (second task)))
        ) tasks))

(defun get-tasks-of-tag(rule &aux tasks)
    (dolist (cur-task defined-tasks tasks)
        (if (find rule (third cur-task))
            (setf tasks (cons cur-task tasks)))))

(defun get-tasks-with-condition(condition &aux tasks)
    ;; ((fboundp something) = T) => (something = function)
    (if (fboundp condition)
        (mapcar #'(lambda(task)
            (if (funcall condition task)
                (setf tasks (cons task tasks)))) defined-tasks)
        (setf tasks (get-tasks-of-tag condition)))
    tasks)

(defun view(&rest rules)
    (format t "~%")
    (view-tasks (cond
        ((eq (length rules) 0) defined-tasks)
        ((eq (length rules) 1) (get-tasks-with-condition (first rules)))
        ((eq (length rules) 2) (union (get-tasks-with-condition (first rules)) (get-tasks-with-condition (second rules))))
        ((>  (length rules) 2) (let ((tasks (get-tasks-with-condition (first rules))))
            (dolist (cur (rest rules) tasks)
                (setf tasks (union tasks (get-tasks-with-condition cur))))))))
    (format t "~%")
    (values))