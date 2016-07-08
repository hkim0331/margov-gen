#|

分かち書きされた日本語文書からこれから拡張 n-gram を作り、
会話する。
hkimura, 2016-07-07.

|#

(in-package :cl-user)
(defpackage :n-gram-ex (:use :cl))
(in-package :n-gram-ex)

(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defun split (string &optional (char #\Space))
  "分かち書きされた文字列 string をスペースで区切ってリストにする。"
  (remove-if #'(lambda (s) (string= "" s)) (split-by-char string char)))

(defvar *s* "髪 を 買っ て ください ます か。" )

(split *s*)

(defun n-gram-ex (xs &optional (n 2))
  (labels ((dup (n xs ret)
             (let ((bt (rest xs)))
               (if (= n 1) ret
                   (dup (- n 1) bt (cons bt ret)))))
           (n-gram-aux (m)
             (if (null (car m)) nil
                 (cons (mapcar #'car m)
                       (n-gram-aux (mapcar #'cdr m))))))
    (mapcar #'reverse (n-gram-aux (dup n xs (list xs))))))

(n-gram-ex (split *s*) 3)

(defvar *dic* "dic-ex.lisp")

;;FIXME: ダサすぎ。もっといい解があるはず。
(defun append-to-file (sexp &optional (fname *dic*))
  (if (probe-file fname)
      (with-open-file (out fname :direction :output :if-exists :append)
        (print sexp out))
      (with-open-file (out fname :direction :output)
        (print sexp out))))

(defun make-n-gram-ex (infile &optional (n 2))
  "infile は分かち書きされた日本語文書。各行は句点（。）で終了していること。"
  (with-open-file (in infile)
    (loop for line = (read-line in nil) while line
       do (unless (string= "" line)
            (append-to-file (n-gram-ex (split line) n))))))

(defvar *s2* "ミスタージェームズディリンガムヤング 。 " )
(n-gram-ex (split *s2*) 2)

(make-n-gram-ex "sample.wakati")

(defvar *n-gram-ex* nil)

(defun load-dic (&optional (fname *dic-ex*))
  "ファイルにセーブした n-gram を *n-gram-ex* に読み込む。"
  (setf *n-gram-ex* nil)
  (with-open-file (in fname)
    (loop for line = (read in nil) while line
         do (setf *n-gram-ex* (nconc *n-gram-ex* line)))))

(load-dic)

(length *n-gram-ex*)

(defvar *end* "。")

(defun top (s)
  (subseq s 0 1))

(defun end? (word)
  (string= *end* (top (reverse word))))

(end? *end*)

(defun generate-ex (w)
  "スタートワード w から出現頻度にもとづき文を生成。
候補が見つからない時は *n-gram-ex* 辞書からランダムにチョイス。"
  (labels
      ((M (s ret)
         (let*
             ((words
               (remove-if-not #'(lambda (x) (string= s (car x))) *n-gram-ex*))
              (word
               (if (null words) (nth (random (length *n-gram-ex*)) *n-gram-ex*)
                   (nth (random (length words)) words))))
           (cond
             ((end? word) (cons *end* (cons word ret)))
             (t (M (top (reverse  word)) (cons word ret)))))))
    (cat (reverse (mapcar #'top (M s nil))))))

(defun prep (infile)
  (let ((temp #p"temp.txt"))
    (prep-text-file infile temp)
    (make-n-gram temp)
    (load-dic)))

;; ;;
;; ;; example
;; (prep #p"sample.txt")
;; (generate "親")
;; (generate "実")
;; (generate "小")
