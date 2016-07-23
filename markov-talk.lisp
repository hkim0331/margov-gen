#|

produce meaningless conversations according to the theory of markov-chain.
by hkimura,
in 2016-07-07, 2016-07-08, 2016-07-09, 2016-07-18,

* 2016-07-22, 最初に読み込んだ辞書をもとに応答分を作成し、読み上げる。
* 2016-07-23, 設計変更。ファイルに n-gram を書き出すのをやめる。

|#

(in-package :cl-user)
(ql:quickload '(cl-ppcre trivial-shell))
(defpackage :markov-talk (:use :cl))
(in-package :markov-talk)

;; constants

;; 辞書。具体的には分かち書きした単語の 2-gram 化リスト。
(defvar *n-gram-ex*)

;; 文の終端となる文字列。句点。
(defvar *end* "。")

;; utilities
(defun top (s)
  (subseq s 0 1))

(defun cat (ss)
  "文字列のリストを引数に取り、それらを連結した文字列を返す。"
  (apply #'concatenate 'string ss))

(defun range (i &optional j k)
  "連続またはステップごとの範囲。
使用例:
(range 5) => (0 1 2 3 4)
(range 1 5) => (1 2 3 4)
(range 1 5 2) => (1 3)"
  (labels
      ((RNG (from to step)
         (labels ((R (from to step ret)
                    (if (>= from to) (nreverse ret)
                        (R (+ from step) to step (cons from ret)))))
           (R from to step nil))))
    (cond
      ((null j) (RNG 0 i 1))
      ((null k) (RNG i j 1))
      (t (RNG i j k)))))

(defun drop (n xs)
  "リスト xs の先頭の n 要素を落とした要素を返す。"
  (cond
    ((zerop n) xs)
    ((null xs) nil)
    (t (drop (- n 1) (cdr xs)))))

(defun take (n xs)
  "リスト xs の先頭の n 要素を返す。"
  (labels
      ((TK (n xs ret)
         (cond
           ;; order is important.
           ((zerop n) (nreverse ret))
           ((null xs) nil)
           (t (TK (1- n) (cdr xs) (cons (car xs) ret))))))
    (TK n xs nil)))

(defun partition (xs n &optional (d n))
  "リスト xs を n 個の要素をもつサブリストに分ける。
(partition '(1 2 3 4) 2) => ((1 2) (3 4))
(partition '(1 2 3 4) 2 1) => ((1 2) (2 3) (3 4))"
  (labels
      ((PA (xs n d ret)
         (let ((head (take n xs)))
           (if (or (null xs) (null head)) (nreverse ret)
               (PA (drop d xs) n d (cons head ret))))))
    (PA xs n d nil)))

(defun run-cmd (cmd &rest args)
  "コマンドを実行。コマンドを絶対パス指定するとエラーの理由は?"
  (labels
      ((L-TO-S (l)
         (if (null l) ""
             (concatenate 'string (car l) " " (L-TO-S (cdr l))))))
    (trivial-shell:shell-command
     (concatenate 'string cmd " " (L-TO-S args)))))

(defun say (text)
  (run-cmd "say" text))

(defun mecab (text)
  (run-cmd
   (format nil "echo ~a | mecab --output-format-type=wakati" text)))

;; n-gram functions
(defun n-gram-ex (xs &optional (n 2))
  (partition xs n 1))

(defun n-gram-from-string (string &optional (n 2))
  "文字列 string を n-gram-ex 化したリストを返す。"
  (n-gram-ex (cl-ppcre:split "\\s" (mecab string)) n))

(defun n-gram-from-file (infile &optional (n 2))
  "infile は普通の日本語テキストファイル。
各行を拡張 n-gram に変換し、一つのリストにまとめて返す。"
  (let ((ret nil))
    (with-open-file (in infile)
      (loop
         :for line = (read-line in nil)
         :while line
         :do (setf ret (nconc ret (n-gram-from-string line n)))))
    ret))

;; base は拡張子を含まないファイル名。
(defun db-load (&optional (base "db"))
  "db.lisp, db.txt の順にデータを読み、あればそのファイルを読んで、
*n-gram-ex* に 2-gram 化したリストをセットする。
成功は t、失敗は nil を返す。"
  (let ((lisp (concatenate 'string base ".lisp"))
        (txt (concatenate 'string base ".txt")))
    (cond
      ((probe-file lisp)
       (progn
         (setf *n-gram-ex* (with-open-file (in lisp) (read in))) t))
      ((probe-file txt)
       (progn
         (setf *n-gram-ex* (n-gram-from-file txt)) t))
      (t nil))))

;; t は余計か？
(defun db-save (&optional (fname "db.lisp"))
  (with-open-file (out fname :direction :output :if-exists :supersede)
    (print *n-gram-ex* out))
  t)

;; ここは nreverse ではまずいだろ。
(defun end? (word)
  (string= *end* (car (reverse word))))

(defun generate-ex (word &optional (dic *n-gram-ex*))
  "スタートワード word から出現頻度にもとづき文を生成。
候補が見つからない時は辞書からランダムにチョイス。"
  (labels
      ((G (w ret)
         (let*
             ((words
               (remove-if-not #'(lambda (x) (string= w (car x))) dic))
              (word
               (if (null words) (nth (random (length *n-gram-ex*)) dic)
                   (nth (random (length words)) words))))
           (cond
             ((end? word) (nreverse (cons (list *end*) (cons word ret))))
              ;; not cdr. for use of 3-gram, 4-gram, etc.
             (t (G (car (reverse word)) (cons word ret)))))))
    (G word nil)))

(defun display (ret)
  (cat (mapcar #'car ret)))

;; try.
;; (display (generate-ex "小銭"))
;; (display (generate-ex "八百屋"))
;; (display (generate-ex "クリスマス"))

(defun is-start-kanji? (word)
  "文字列 word の先頭文字が漢字かどうか。廣瀬のプログラムを盗む。"
  (labels ((IN? (c low up) (and (<= low c) (<= c up))))
    (let ((cc (char-code (coerce (subseq word 0 1) 'character))))
      (IN? cc 19968 65280))))

(defun words (string)
  "文字列 string を空白文字で区切ったリストに変換"
  (cl-ppcre:split "\\s" (mecab string)))

;; FIXME: first で決め打ちはよくない。乱数で揺らすか?
(defun key-word (words)
  (first (remove-if-not #'is-start-kanji? words)))

(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun talk-1 (prompt)
  "質問文の言葉尻をとらえ、文章を生成する。質問文を辞書に動的に追加する。"
  (let* ((line (prompt-read prompt))
         (key-word (key-word (words line))))
    (prog1
        (display (generate-ex key-word))
      ;;FIXME, グローバル変数が丸見え。よくない。
      (setf *n-gram-ex* (nconc (n-gram-from-string line) *n-gram-ex*)))))

;; try.
;; (talk-1 "? ")

;; 会話にする。
(defun lets-talk (&optional (db "db"))
  (format t "now loading ~a ...~%" db)
  (db-load db)
  (loop
     (say (talk-1 "talk: "))
     (if (y-or-n-p "会話をやめますか?") (return)))
  (db-save (concatenate 'string db ".lisp"))
  (format t "see you later.~%"))

;; お笑いの始まり。
;;(lets-talk)
