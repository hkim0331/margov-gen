 #|

分かち書きされた日本語文書から拡張 n-gram を作り、会話する。
mecab により、分かち書きされたテキストファイルを入力とする。
n-gram 化したリストの各要素は markov-talk では次になる。

("親譲" "譲り" "りの" "の無" "無鉄" "鉄砲" "砲で" "で小" "小供" "供の" "の時" "時か" "から" "ら損" "損ば" "ばか" "かり" "りし" "して" "てい" "いる" "る。")

n-gram-ex では次。

(("親譲り" "の") ("の" "無鉄砲") ("無鉄砲" "で") ("で" "小") ("小" "供") ("供" "の") ("の" "時") ("時" "から") ("から" "損") ("損" "ばかり") ("ばかり" "し") ("し" "て") ("て" "いる") ("いる" "。"))

hkimura, 2016-07-07, 2016-07-08, 2016-07-09,

|#

(in-package :cl-user)
(defpackage :n-gram-ex (:use :cl))
(in-package :n-gram-ex)

(defun range (i &optional j k)
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
  (cond
    ((zerop n) xs)
    ((null xs) nil)
    (t (drop (- n 1) (cdr xs)))))

(defun take (n xs)
  (labels
      ((TK (n xs ret)
         (cond
           ;; order is important.
           ((zerop n) (nreverse ret))
           ((null xs) nil)
           (t (TK (1- n) (cdr xs) (cons (car xs) ret))))))
    (TK n xs nil)))

(defun partition (xs n &optional (d n))
  (labels
      ((PA (xs n d ret)
         (let ((head (take n xs)))
           (if (or (null xs) (null head)) (nreverse ret)
               (PA (drop d xs) n d (cons head ret))))))
    (PA xs n d nil)))

(defun n-gram-ex (xs &optional (n 2))
  (partition xs n 1))

;; FIXME: cl:ppcre で書き直せないか?
(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defun split (string &optional (char #\Space))
  "分かち書きした文字列 string を charで区切ってリストにする。
char を省略した場合 #\Space で区切る。"
  (remove-if #'(lambda (s) (string= "" s)) (split-by-char string char)))

;;(defvar *s2* "親譲り の 無鉄砲 で 小 供 の 時 から 損 ばかり し て いる 。" )
;; (n-gram-ex (split *s2*) 2)
;; ;; => (("親譲り" "の") ("の" "無鉄砲") ("無鉄砲" "で") ("で" "小") ("小" "供") ("供" "の") ("の" "時") ("時" "から") ("から" "損") ("損" "ばかり") ("ばかり" "し") ("し" "て") ("て" "いる") ("いる" "。"))

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

;; ここは nreverse ではまずいだろ。
(defun end? (word)
  (string= *end* (car (reverse word))))

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
             ((end? word) (nreverse (cons (list *end*) (cons word ret))))
              ;; not cdr. for use of 3-gram, 4-gram, etc.
             (t (G (car (reverse word)) (cons word ret)))))))
    (G w nil)))

(defun cat (ss)
  "文字列のリストを引数に取り、それらを連結した文字列を返す。"
  (apply #'concatenate 'string ss))

(defun display (ret)
  (cat (mapcar #'car ret)))

(make-n-gram-ex #p"data/賢者の贈り物.mecab")
(load-dic-ex)

(display (generate-ex "わたし"))
(display (generate-ex "髪"))
(display (generate-ex "櫛"))
(display (generate-ex "時計"))

;; sbcl only.
(defun run-cmd (cmd &rest args)
  (with-output-to-string (out)
    (sb-ext:run-program cmd args :output out)))

(defun say (text)
  (run-cmd "/usr/bin/say" text))

;; first version, use temporaly file.
;; 未完。
;; run-cmd ではパイプを使えない。
;; パイプでつないだコマンドをシェルスクリプトにしておくか。
;; with-input-from-string を使えないか？
(defun mecab (text)
  (let ((tmp "tempfile"))
    )
  )

;; 動作を確認できたらまとめちゃってもいい。
;; 一つの関数にまとめる。

(defun prompt-read (prompt)
  (formt *query-io* "~a:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; n この候補を出し、入力文書と m 個以上共通項のある文書を出力する。
;; ``共通''の意味は最初はひとまず``等しい''でよい。将来、``近い''に変更する。

;; 引数を文、あるいは文中に含まれる ``文の特徴を表すワード''にしたい。
(defun talk-1 (prompt)
  "word を引いて文章を生成する。"
  (display (generate-ex  (prompt-read prompt))))

;; 会話にする。
(defun lets-talk (&optional (ngram #p "data/賢者の贈り物.mecab"))
  (make-n-gram-ex ngram)
  (load-dic-ex)
  (loop (talk-1 "talk: ")
     (if (not-y-or-n-p "continue? [y/n]: ")) (return)))


