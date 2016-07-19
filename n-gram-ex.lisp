#|
分かち書きされた日本語文書から拡張 n-gram を作り、会話する。
mecab により、分かち書きされたテキストファイルを入力とする。

n-gram 化したリストの各要素は markov-talk では次になる。
("親譲" "譲り" "りの" "の無" "無鉄" "鉄砲" "砲で" "で小" "小供" "供の" "の時" "時か" "から" "ら損" "損ば" "ばか" "かり" "りし" "して" "てい" "いる" "る。")

n-gram-ex では次。
(("親譲り" "の") ("の" "無鉄砲") ("無鉄砲" "で") ("で" "小") ("小" "供") ("供" "の") ("の" "時") ("時" "から") ("から" "損") ("損" "ばかり") ("ばかり" "し") ("し" "て") ("て" "いる") ("いる" "。"))

hkimura, 2016-07-07, 2016-07-08, 2016-07-09, 2016-07-18,
|#

(in-package :cl-user)
(ql:quickload :cl-ppcre)
(defpackage :n-gram-ex (:use :cl))
(in-package :n-gram-ex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sbcl only.
(defun run-cmd (cmd &rest args)
  (with-output-to-string (out)
    (sb-ext:run-program cmd args :output out)))

(defun say (text)
  (run-cmd "/usr/bin/say" text))

;; run-cmd ではパイプを使えない。
;; パイプでつないだコマンドをシェルスクリプトにしておくか。
;;
;;(mecab "今日は天気がいい。")
;; "今日 は 天気 が いい 。 
;; "
;; 最後の空白と改行が余計。
(defun mecab (text)
   (run-cmd "./mecab.sh" text))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (i &optional j k)
  "連続またはステップごとの範囲。
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

;; 拡張 n-gram。
(defun n-gram-ex (xs &optional (n 2))
  (partition xs n 1))

(defvar *dic-ex* "dic-ex.lisp")

(defun append-to-file (sexp &optional (fname *dic-ex*))
  (unless (probe-file fname)
    (with-open-file (out fname :direction :output)))
  (with-open-file (out fname :direction :output :if-exists :append)
        (print sexp out)))

(defun n-gram-from-string (string)
  "文字列 string を n-gram-ex 化したリストを返す。
string が句点終了しない場合、句点を補う（つもり）。"
  (n-gram-ex (cl-ppcre:split "\\s" (mecab string)) 2))

(defun n-gram-from-stream (st)
  "ストリーム st から一行読んで、n-gram-ex 化したリストを返す。"
  (let ((line (read-line st nil)))
    (if (null line) nil
        (n-gram-from-string line))))

;;FIXME: 句点終了していなかったら、句点をアペンドする。
;;FIXME: ファイル以外、標準入力から入力を取れるように。
(defun make-n-gram-ex (infile &optional (n 2))
  "infile は分かち書きされた日本語テキストファイル。各行は句点（。）で終了していること。
各行を拡張 n-gram に変換し、 *dic-ex* で示すファイルに書き出す。"
  (with-open-file (in infile)
    (loop
       :for line = (read-line in nil)
       :while line
       :do (append-to-file (n-gram-ex (cl-ppcre:split "\\s" line) n)))))

(defvar *n-gram-ex* nil)

;;
(defun load-dic-ex (&optional (fname *dic-ex*))
  "fname ファイルにセーブした n-gram を *n-gram-ex* に読み込む。
fname を省略すると *dic-ex* から読み込む。"
  (setf *n-gram-ex* nil)
  (with-open-file (in fname)
    (loop
       :for line = (read in nil)
       :while line
       :do (setf *n-gram-ex* (nconc line *n-gram-ex*)))))

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

;; try.
(display (generate-ex "わたし"))
(display (generate-ex "髪"))
(display (generate-ex "櫛"))
(display (generate-ex "時計"))

;; 動作を確認できたらまとめちゃってもいい。

;; 会話にする。
(defun lets-talk (dic)
  )
;; 音声入出力。

;; 一つの関数にまとめる。

(defun prompt-read (prompt)
  (format *query-io* "~a:" prompt)
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
  (loop
     (talk-1 "talk: ")
     (if (y-or-n-p "continue? [y/n]: ") (return))))
