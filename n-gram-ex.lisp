#|

分かち書きされた日本語文書からこれから拡張 n-gram を作り、
会話する。

hkimura, 2016-07-07.

|#

(in-package :cl-user)
(defpackage :n-gram-ex (:use :cl))

(in-package :n-gram-ex)

(defvar *s* "  髪 を 買っ て ください ます か 。  " )

(defun split (string)
  (labels ((split-by-one-space (string)
             (loop for i = 0 then (1+ j)
                as j = (position #\Space string :start i)
                collect (subseq string i j)
                while j)))
    (remove-if #'(lambda (s) (string= "" s)) (split-by-one-space string))))

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

(n-gram-ex (split *s*))

(defvar *dic* "dic.lisp")

;;FIXME: ダサすぎ。もっといい解があるはず。
(defun append-to-file (sexp &optional (fname *dic*))
  (if (probe-file fname)
      (with-open-file (out fname
                           :direction :output
                           :if-exists :append)
        (print sexp out))
      (with-open-file (out fname
                           :direction :output)
        (print sexp out))))

(defun make-n-gram-ex (infile &optional (n 2))
  "infile は分かち書きされた日本語文書。各行は句点（。）で終了していること。"
  (with-open-file (in infile)
    (loop for line = (read-line in nil) while line
       do (unless (string= "" line)
            (append-to-file (n-gram-ex line n))))))

;; ;; ここから下がまだ。続きは帰宅後。
;; (make-n-gram-ex "sample.wakati")

;; (defvar *n-gram* nil)

;; (defun load-dic (&optional (fname *dic*))
;;   "ファイルにセーブした n-gram を読み込む。"
;;   (setf *n-gram* nil)
;;   (with-open-file (in fname)
;;     (loop for line = (read in nil) while line
;;          do (setf *n-gram* (nconc *n-gram* line)))))

;; (defvar *end* "。")

;; (defun end? (word)
;;   (string= *end* (top (reverse word))))

;; (defun generate (s)
;;   "スタート文字 s から出現頻度にもとづき文を生成。
;; 見つからない時は n-gram 辞書からランダムにチョイス。"
;;   (labels
;;       ((M (s ret)
;;          (let* ((words (remove-if-not #'(lambda (x) (string= s (top x))) *n-gram*))
;;                 (word (if (null words) (nth (random (length *n-gram*)) *n-gram*)
;;                           (nth (random (length words)) words))))
;;            (cond
;;              ((end? word) (cons *end* (cons word ret)))
;;              (t (M (top (reverse  word)) (cons word ret)))))))
;;     (cat (reverse (mapcar #'top (M s nil))))))

;; (defun prep (infile)
;;   (let ((temp #p"temp.txt"))
;;     (prep-text-file infile temp)
;;     (make-n-gram temp)
;;     (load-dic)))

;; ;;
;; ;; example
;; (prep #p"sample.txt")
;; (generate "親")
;; (generate "実")
;; (generate "小")




