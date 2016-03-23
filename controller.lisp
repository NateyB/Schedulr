(defvar defined-tasks `())
(defvar defined-tags `())
(defconstant task-help
    (format `NIL "~%~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%
                  ~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~%
                  ~%~A~%"

        "Commands:"
        "(load-tasks-from-file): Loads the defined-tasks from the file"
        "(add name `(year month day) tag1 tag2 ...) OR (add name `(year month day) `(tag1 tag2 ...)) will add a task to the list"
        "(remove-task name): Removes a task from the list of tasks"
        "(view tag1 tag2 ...): Views union of the tasks associated with each tag or lambda"
        "(view* tag1 tag2 ...): Same as view, but includes the tags/rules included"
        "(mark-completed name): Marks the oldest task with name 'name' as completed"
        "(flush): Writes the updated tasks to file"
        "(modify task-name which-value &rest new-values): Modifies task-name based on which-value with new-value"

        "Generic Assignmenth Rules:"
        "'overdue: Views tasks that are overdue"
        "'today: Views tasks that are due today or overdue"
        "'tomorrow: Views tasks that are due tomrrow (but not overdue)"
        "'week: Views tasks that are due this week (or overdue)"
        "'all: View all tasks"
        "(tag-difference `(list of tags) `(list of others)): Returns a lambda expression that returns true if the task is in the list of tags, but not others"
        "(tag-intersection tag1 tag2 tag3 ...): Returns a lambda expression that returns true if the task is all of the tags"

        "For more information about a function, use the documentation command."))
(defvar saved-object `NIL)


(defun argmax(set toMaximize &optional get-val? &aux (max `NIL))
    "argmax(set toMaximize &optional get-vall?)
    Using items in 'set', find the argument that maximizes the 'toMaximize' expression;
    if get-val is specified, returns a tuple (the argument, its value)."
    (dolist (cur set (or (and get-val? max) (first max)))
        (let ((val (funcall toMaximize cur)))
            (if (or (not max) (> val (second max)))
                (setf max `(,cur ,val))))))

(defun add(name due-date tags &optional (on-check 'mark-completed))
    "add(name due-date tag1 tag2 ...) OR add(name due-date (tags))
    Adds the task (specified by the parameters) to the database. Also adds the tags to the list
    of tags, defined-tags."
    (when (not (listp tags))
        (setf tags (listp tags)))
    (setf defined-tasks (cons `(,name ,due-date ,tags ,on-check) defined-tasks))
    (setf defined-tags  (union tags defined-tags)))

(defun select-newest(tasks)
    "select-newest(tasks)
    Given the list of tasks, 'tasks', select the newest component by time."
    (argmax tasks #'(lambda(x)
        (- (+ (* 1000 (first (second x))) (* 100 (second (second x))) (third (second x)))))))

(defun get-oldest-by-name(name &aux ties)
    "get-oldest-by-name(name)
    Gets the oldest task with name equal to 'name'."
    (select-newest (remove-if-not (lambda(task) (equal (first task) name)) defined-tasks)))

(defun remove-task(name &optional date &aux (task (get-task name date)))
    "remove-task(name)
    Removes the oldest task named 'name'."
    (setf defined-tasks (remove task defined-tasks)))

(defun show-tasks (tasks &optional show-tags)
    "show-tasks(tasks &optional show-tags)
    Organizes and displays every task in a given list of tasks. If show-tags is specified, then
    appends the list of tags to the lines."
    (mapcar #'(lambda(item) (format t "~A~&" item)) (sort (mapcar #'(lambda(task) (format `NIL "~4D-~2,'0D-~2,'0D: ~A ~A" (first (second task)) (second (second task)) (third (second task)) (first task) (if show-tags (third task) ""))) tasks) #'string-lessp))
    (values))

(defun get-tasks-of-tag(tag)
    "get-tasks-of-tag(tag)
    Gets all tasks associated with 'tag'."
    (remove-if-not (lambda(task) (find tag (third task))) defined-tasks))

(defun get-tasks-of-function(rule)
    "get-tasks-of-function(rule)
    Gets all tasks that pass function 'rule'."
    (remove-if-not rule defined-tasks))

(defun pass-condition(condition task)
    "pass-condition(condition task)
    Returns (task meets condition). Requires that condition be a tag or rule. If both a tag and a
    rule, will treat as a tag and not rule."
    (if (find condition defined-tags)
            (find condition (third task))
        (funcall condition task)))

(defun get-tasks-with-condition(condition)
    "get-tasks-with-condition(condition)
    Returns all tasks that meet the condition specified."
    (remove-if-not (lambda(task) (pass-condition condition task)) defined-tasks))

(defun date-to-time(&optional (date (time-to-date)))
    "date-to-time(&optional (date (time-to-date)))
    Returns the universal time corresponding to the start of 'date'."
    (encode-universal-time 0 0 0 (third date) (second date) (first date) (nth-value 8 (get-decoded-time))))

(defun time-to-date(&optional (seconds (get-universal-time)))
    "time-to-date(&optional (seconds (get-universal-time)))
    Returns the date of the universal time in the format `(year month day)."
    (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (decode-universal-time seconds) (declare (ignore sec min hr dow dst-p tz))
    (list yr mon day)))

;; Returns the current day of the week (in numerical form)
(defun day-of-week()
    "day-of-week()
    Returns the current day of the week (in numerical form)."
    (nth-value 6 (get-decoded-time)))

(defun do-the-showing(rules &optional show-tags)
    "do-the-showing(rules &optional show-tags)
    Shows all tasks matching 'rules'. If show-tags is specified, show the tags as well. "
    (show-tasks (cond
        ((eq (length rules) 0) defined-tasks)
        ((eq (length rules) 1) (get-tasks-with-condition (first rules)))
        ((eq (length rules) 2) (union (get-tasks-with-condition (first rules)) (get-tasks-with-condition (second rules))))
        ((>= (length rules) 3) (let ((tasks (get-tasks-with-condition (first rules))))
            (dolist (cur (rest rules) tasks)
                (setf tasks (union tasks (get-tasks-with-condition cur))))))) show-tags))

(defun view(&rest rules)
    "view(&rest rules)
    Views the all tasks associated with the specified rules; if no rules are specified, then views
    all tasks."
    (format t "~%")
    (do-the-showing rules `NIL)
    (format t "~%")
    (values))

(defun view*(&rest rules)
    "view*(&rest rules)
    Same as the 'view' function, but also displays the tags."
    (format t "~%")
    (do-the-showing rules t)
    (format t "~%")
    (values))

(defun get-task(name &optional date)
    (if date
        (first (remove-if-not (lambda(task) (and (equal (first task) name) (equal (second task) date))) defined-tasks))
        (get-oldest-by-name name)))

(defun check(name &optional date &aux (task (get-task name date)))
    (when (fourth task)
        (funcall (fourth task) task))
    (mark-completed task))

(defun disregard(name &optional date &aux (task (get-task name date)))
    (funcall (fourth task) task)
    (remove-task name (second task)))

(defun mark-completed(task)
    "mark-completed(name)
    Moves task to 'completed_tasks.dat' and removes it from the currently defined task."
    (let ((completed-tasks (restore-object "completed_tasks.fasl")))
        (setf completed-tasks (cons task completed-tasks))
        (save-object completed-tasks "completed_tasks.fasl")
        (remove-task (first task))))

(defun flush()
    "flush()
    Writes tasks to the 'user_tasks.fasl' file; if not present, the file is created. If present, the
    contents of the file are overwritten."
    (save-object defined-tasks "user_tasks.fasl"))

(defun save-object(object &optional (file "user_tasks.fasl"))
    (let ((saved-object object))
        (compile-file "user_tasks.lisp" :output-file file)))

(defun restore-object(&optional (file "user_tasks.fasl"))
    (let ((saved-object `NIL))
        (load file)
        saved-object))

(defun load-tasks()
    "load-tasks-from-file()
    Loads tasks from the 'user_tasks.dat' file; if not present, the file is created."
    (setf defined-tasks (restore-object "user_tasks.fasl"))
    (dolist (cur defined-tasks)
        (setf defined-tags (union defined-tags (third cur)))))

(defun modify(task-name which-value &rest new-value &aux (task (get-oldest-by-name task-name)))
    "modify(task-name which-value &rest new-value)
    Modifies task-name according to the value of which-value:
        `(1 'name' 'n')        : Changes the name to the first item of new-value.
        `(2 'date' 'd')        : Changes the date to the first item of new-value.
        `('add-tags' 'addt')   : Adds the tags of new-value to the task's conditions.
        `('remove-tags' 'remt'): Removes the tags of new-value from the task's conditions.
        `('set-tags' 'sett')   : Sets the tags of the task to new-value."
    (let ((name-modifier `(1 "name" "n"))
          (date-modifier `(2 "date" "d"))
          (add-tags `("add-tags" 31 "addt"))
          (remove-tags `("remove-tags" 32 "remt"))
          (set-tags `("set-tags" 33 "sett"))
          (on-check `("on-check" 4 "c")))
        (when task
            (cond
                ((find which-value name-modifier :test 'equal) (setf (first task) (first new-value)))
                ((find which-value date-modifier :test 'equal) (setf (second task) (first new-value)))
                ((find which-value add-tags :test 'equal)      (setf (third task) (union (third task) new-value :test 'equal)))
                ((find which-value remove-tags :test 'equal)   (setf (third task) (set-difference (third task) new-value :test 'equal)))
                ((find which-value set-tags :test 'equal)      (setf (third task) new-value))
                ((find which-value on-check :test 'equal)      (setf (fourth task) (first new-value)))
                (t (format `NIL "Could not comprehend options. Format is (modify task-name which-value new-values...).~%Which-value accepts only items within the following lists~%~A; ~A; ~A; ~A; ~A" name-modifier date-modifier add-tags remove-tags set-tags))))))

;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
;;                                  Generic Assignment Rules                                      ;;
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
(defun overdue(task)
    "True for tasks that are overdue"
    (< (date-to-time (second task)) (date-to-time)))

(defun today(task)
    "True for tasks that are due today or overdue"
    (<= (date-to-time (second task)) (get-universal-time)))

(defun tomorrow(task)
    "True for tasks that are due tomorrow"
    (equal (second task) (time-to-date (+ (get-universal-time) 86400))))

(defun week(task)
    "True for tasks that are overdue and due over the course of the next week"
    (<= (date-to-time (second task)) (+ (get-universal-time) 518400)))

(defun all(task)
    "True for all tasks"
    t)

(defun recurring(task)
    (fourth task))

(defun tag-difference(group1 group2)
    "Given two lists, returns a lambda test that passses only tasks that pass the first group of
    tests, but not the second group."
    (lambda(task)
        (and
            (> (length (remove-if-not (lambda(condition) (pass-condition condition task)) group1)) 0)
            (= (length (remove-if (lambda(condition) (pass-condition condition task)) group2)) (length group2)))))

(defun tag-intersection(&rest tags)
    "Given a list of conditions, returns a lambda test that passes only tasks that pass all of the conditions provided."
    (when (listp (first tags))
        (setf tags (first tags)))
    (lambda(task)
        (= (length (remove-if-not (lambda(condition) (pass-condition condition task)) tags)) (length tags))))



;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
;;                                      Generic-Check Rules                                       ;;
;;                          Note that these must return compiled lambdas                          ;;
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
(defun weekly(&optional date)
    (compile nil #'(lambda(task)
        (when (or (not date) (>= (date-to-time date) (+ (date-to-time (second task)) 604800)))
            (add (first task) (time-to-date (+ (date-to-time (second task)) 604800)) (third task) (weekly date))))))



;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
;;                                User-Defined Assignment Rules                                   ;;
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ;;
(defvar default (tag-difference `(week) `(winter-break)))