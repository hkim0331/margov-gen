 #|

分かち書きされた日本語文書から拡張 n-gram を作り、会話する。
mecab により、分かち書きされたテキストファイルを入力とする。
n-gram 化したリストの各要素は markov-talk では、

("親譲" "譲り" "りの" "の無" "無鉄" "鉄砲" "砲で" "で小" "小供" "供の" "の時" "時か" "から" "ら損" "損ば" "ばか" "かり" "りし" "して" "てい" "いる" "る。")

となるが、こちらの n-gram-ex では、*** 作文途中 ***

隣の markov-talk とファイル名、グローバルシンボルがかぶらないように。

hkimura, 2016-07-07, 2016-07-08,

|#

(in-package :cl-user)
(defpackage :n-gram-ex (:use :cl))
(in-package :n-gram-ex)

;; cl:ppcre で書き直せないか?
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

;;FIXME: Mathematica の Partition みたいな関数を定義できないか?
;;partition (xs &key n (offset n)) な感じ。そうすると、
;;
;;(defun n-gram-ex (xs &optional (n 2))
;;   (partition xs n 1))

(defun n-gram-ex (xs &optional (n 2))
  "入力は単語のリストからなるリスト。"
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
             ((end? word) (cons (list *end*) (cons word ret)))
             ;; not cdr. for use of 3-gram, 4-gram, etc.
             (t (G (car (reverse word)) (cons word ret)))))))
    (reverse (G w nil))))

(defun cat (ss)
  "文字列のリストを引数に取り、それらを連結した文字列を返す。"
  (labels ((cat2 (a b)
             (concatenate 'string a b)))
    (if (null ss) ""
        (cat2 (car ss) (cat (cdr ss))))))

(defun display (ret)
  (cat (mapcar #'car ret)))

(make-n-gram-ex #p"data/賢者の贈り物.mecab")
(load-dic-ex)

;;(display (generate-ex "親譲り"))
;; "親譲りの井戸のナイフを半分崩してきた"
;;
;; "親譲りの甲を失って済んだと肴屋という十三人が湧わき出なくなった事はどんな仕掛か、そこいたら、深く埋めて、栗をぎゅうぎゅう井戸を捕まえて奇麗な栗だが山城屋の地面は菜園のを食っても取り返してぐいぐい押おした。"
;;
;; "親譲りの稲に、とうとう勘太郎は無論弱虫の夕方折戸のナイフが堅かったの中にある"

;;(display (generate-ex "実"))
;; "実の甲を失って済んだと肴屋という十三人が湧わき出なくなった事はどんな仕掛か、そこいたら、深く埋めて、栗をぎゅうぎゅう井戸を捕まえて奇麗な栗だが山城屋の地面は菜園のを食っても取り返してぐいぐい押おした。"
;;
;; "実の節を抜ぬかしたので、右左へ挿し込んで食う。"
;;
;; "実の人参畠をつれてみろと答えた。"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; N-GRAM-EX> (display (GENERATE-EX "髪"))
;; "髪はね、はじめの髪はね、変わらずに好きで。"
;; N-GRAM-EX> (display (GENERATE-EX "髪"))
;; "髪ができるっていうの。"
;; N-GRAM-EX> (display (GENERATE-EX "髪"))
;; "髪ができるのうちしばらく、わたしのことは、でしょうけれど。"
;; N-GRAM-EX> (display (GENERATE-EX "時計"))
;; "時計、ねえ素敵なプレゼントはまた伸びるわ——ああ、あなたを探してよ。"
;; N-GRAM-EX> (display (GENERATE-EX "時計"))
;; "時計、わたしのかわいいと思うよ。"
;; N-GRAM-EX> (display (GENERATE-EX "時計"))
;; "時計になるか見たいの間、あなたにプレゼント一つあげずにしないんだ！。"
;; N-GRAM-EX> (display (GENERATE-EX "時計"))
;; "時計に好きで。"
;; N-GRAM-EX> (display (GENERATE-EX "時計"))
;; "時計を火に、一本一日に１ドル８７セントで。"
;; N-GRAM-EX> (display (GENERATE-EX "時計"))
;; "時計を作るためになったりするもんか。"
;; N-GRAM-EX> (display (GENERATE-EX "櫛"))
;; "櫛を売っちゃったのことは上等すぎるよ、ジム！。"
;; N-GRAM-EX> (display (GENERATE-EX "櫛"))
;; "櫛を作るためになるわ——綺麗で。"
;; N-GRAM-EX> (display (GENERATE-EX "櫛"))
;; "櫛を過ごすなんて絶対できないで僕のことをどれだけ愛して、よね？。"
;; N-GRAM-EX> (display (GENERATE-EX "櫛"))
;; "櫛を切ってもいい、ジムができるっていうのは、チョップをつけたら、売っちゃったのうちしばらく、貸してもいいのよ。"

