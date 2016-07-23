#|
分かち書きされた日本語文書から拡張 n-gram を作り、会話する。
mecab により、分かち書きされたテキストファイルを入力とする。

n-gram 化したリストの各要素は markov-talk では次になる。
("親譲" "譲り" "りの" "の無" "無鉄" "鉄砲" "砲で" "で小" "小供" "供の" "の時" "時か" "から" "ら損" "損ば" "ばか" "かり" "りし" "して" "てい" "いる" "る。")

n-gram-ex では次。
(("親譲り" "の") ("の" "無鉄砲") ("無鉄砲" "で") ("で" "小") ("小" "供") ("供" "の") ("の" "時") ("時" "から") ("から" "損") ("損" "ばかり") ("ばかり" "し") ("し" "て") ("て" "いる") ("いる" "。"))

hkimura, 2016-07-07, 2016-07-08, 2016-07-09, 2016-07-18,

* 2016-07-22, 最初に読み込んだ辞書をもとに応答分を作成し、読み上げる。
* 2016-07-23, 設計変更。ファイルに n-gram を書き出すのをやめる。

|#

(in-package :cl-user)
(ql:quickload '(cl-ppcre trivial-shell))
(defpackage :markov-talk (:use :cl))
(in-package :markov-talk)

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

(defvar *n-gram-ex* (n-gram-from-file "sample.txt"))

(defvar *end* "。")

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

(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

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

(defun extend-dic-by (ngram &optional (dic *n-gram-ex*))
  (setf dic (nconc ngram dic)))

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
(defun lets-talk ()
  (loop
     (say (talk-1 "talk: "))
     (if (y-or-n-p "会話をやめますか?") (return))))
;;
;; お笑いの始まり。
;;
;;(lets-talk)
