(load "cluster.lisp")
(load "distribution.lisp")

;; Generate some normally distributed point clouds
(defvar streams-1 (distribution:generate-random-streams 20 '(10 10) '(1 1)))
(defvar streams-2 (distribution:generate-random-streams 20 '(1 100) '(5000 1000)))
(defvar streams-3 (distribution:generate-random-streams 20 '(60 10) '(1000 10)))

(defun generate-table (streams no-lines)
  (distribution:generate-list (lambda () (distribution:generate-distributed-point streams)) no-lines))


(defvar table-1 (generate-table streams-1 100))
(defvar table-2 (generate-table streams-2 200))
(defvar table-3 (generate-table streams-3 500))

(defvar shuffled (distribution:shuffle table-1 table-2 table-3))


