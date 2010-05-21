;;;; A translation of Practical Common Lisp Chapter 3 from Common Lisp to Clojure
;;;; http://www.gigamonkeys.com/book/practical-a-simple-database.html
;;;;
;;;; John David Eriksen
;;;; jkndrkn@gmail.com

(ns pcl-03
  (:use clojure.contrib.duck-streams))

(def *db* (ref ()))

(defstruct cd :title :artist :rating :ripped)

(defn make-cd [title artist rating ripped]
  (struct cd title artist rating ripped))

(defn add-record [cd]
  (dosync (alter *db* conj cd)))

(defn dump-db []
  (map
   #(println
     (format "TITLE: %s%nARTIST: %s%nRATING: %s%nRIPPED:%s%n" 
	     (:title %) (:artist %) (:rating %) (:ripped %)))
   (deref *db*)))

(defn prompt-read [prompt]
  (print (format "%s: " prompt))
  (flush)
  (read-line))

(defn parse-integer [str]
  (try (Integer/parseInt str)
       (catch NumberFormatException nfe 0)))

(defn y-or-n [str]
  (let [first (first str)]
    (= first \y)))

(defn prompt-for-cd []
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (parse-integer (prompt-read "Rating"))
   (y-or-n (prompt-read "Ripped [y/n]"))))

(defn add-cds []
  (loop []
    (add-record (prompt-for-cd))
    (if (y-or-n (prompt-read "Another?"))
      (recur))))
		
(defn save-db [filename]
  (spit filename (deref *db*)))
  
(defn load-db [filename]
  (dosync (ref-set *db* (read-string (slurp filename)))))

(defn select [selector]
  (filter selector (deref *db*)))

(defn where [clause]
  #(and
    (if (:title clause) (= (:title clause) (:title %)) true)
    (if (:artist clause) (= (:artist clause) (:artist %)) true)
    (if (:rating clause) (= (:rating clause) (:rating %)) true)
    (if (:ripped clause) (= (:ripped clause) (:ripped %)) true)))

(defn update [selector values]
  (dosync
   (ref-set
    *db*
    (map
     #(if (selector %)
	(merge
	 (if (:title values) (merge % values))
	 (if (:artist values) (merge % values))
	 (if (:rating values) (merge % values))
	 (if (:ripped values) (merge % values)))
	%)
     (deref *db*)))))

(defn delete [selector]
  (dosync (ref-set *db* (remove #(selector %) (deref *db*)))))

(defn make-comparison-expr [field clause]
  `(= (~field ~clause) (~field ~'value)))

(defn make-comparisons-list [clauses]
  (for [[f v] clauses]
    (let [clause (hash-map f v)]
      (make-comparison-expr f clause))))

(defmacro where-macro [clauses]
  `(fn [~'value] (and ~@(make-comparisons-list clauses))))
