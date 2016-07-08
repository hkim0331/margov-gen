#|

分かち書きされた日本語文書からこれから拡張 n-gram を作り、
会話する。
隣の markov-talk とファイル名、グローバルシンボルがかぶらないように。
mecab により、分かち書きされたテキストファイルを入力とする。
n-gram 化したリストの各要素は markov-talk では、

("親譲" "譲り" "りの" "の無" "無鉄" "鉄砲" "砲で" "で小" "小供" "供の" "の時" "時か" "から" "ら損" "損ば" "ばか" "かり" "りし" "して" "てい" "いる" "る。")

となるが、こちらの n-gram-ex では、*** 作文途中 ***

hkimura, 2016-07-07, 2016-07-08,

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
  "分かち書きされた文字列 string を charで区切ってリストにする。
char を省略した場合 #\Space で区切る。"
  (remove-if #'(lambda (s) (string= "" s)) (split-by-char string char)))

(defvar *s* "髪 を 買っ て ください ます か。" )
(split *s*)
;;=> ("髪" "を" "買っ" "て" "ください" "ます" "か。")

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

(defvar *s2* "親譲り の 無鉄砲 で 小 供 の 時 から 損 ばかり し て いる 。" )
(n-gram-ex (split *s2*) 2)
;; => (("親譲り" "の") ("の" "無鉄砲") ("無鉄砲" "で") ("で" "小") ("小" "供") ("供" "の") ("の" "時") ("時" "から") ("から" "損") ("損" "ばかり") ("ばかり" "し") ("し" "て") ("て" "いる") ("いる" "。"))

(defvar *dic-ex* "dic-ex.lisp")

;;FIXME: ダサすぎ。もっといい解があるはず。
(defun append-to-file (sexp &optional (fname *dic-ex*))
  (if (probe-file fname)
      (with-open-file (out fname :direction :output :if-exists :append)
        (print sexp out))
      (with-open-file (out fname :direction :output)
        (print sexp out))))

(defun make-n-gram-ex (infile &optional (n 2))
  "infile は分かち書きされた日本語テキストファイル。
各行は句点（。）で終了していること。"
  (with-open-file (in infile)
    (loop for line = (read-line in nil) while line
       do (unless (string= "" line)
            (append-to-file (n-gram-ex (split line) n))))))

(defvar *n-gram-ex* nil)

(defun load-dic-ex (&optional (fname *dic-ex*))
  "ファイルにセーブした n-gram を *n-gram-ex* に読み込む。"
  (setf *n-gram-ex* nil)
  (with-open-file (in fname)
    (loop for line = (read in nil) while line
         do (setf *n-gram-ex* (nconc *n-gram-ex* line)))))

(defun top (s)
  (subseq s 0 1))

(defvar *end* "。")
(defun end? (word)
  (string= *end* word))

(make-n-gram-ex #p"data/坊っちゃん.wakati")

(load-dic-ex)
(length *n-gram-ex*)
(end? *end*)

;; start-from ?
(defun generate-ex (w)
  "スタートワード w から出現頻度にもとづき文を生成。
候補が見つからない時は *n-gram-ex* 辞書からランダムにチョイス。"
  (labels
      ((G (w ret)
         (let*
             ((words
               (remove-if-not #'(lambda (x) (string= w (car x))) *n-gram-ex*))
              (word
               (if (null words) (nth (random (length *n-gram-ex*)) *n-gram-ex*)
                   (nth (random (length words)) words))))
           (cond
             ((end? word) (cons *end* (cons word ret)))
             (t (G (top (reverse  word)) (cons word ret)))))))
    (G w nil)))

;; FIXME error:
;; the value of ("学校" "で") is not of the expected type
(generate-ex "学校")

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
