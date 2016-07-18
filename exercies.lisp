(defun load-dic-ex-0 (&optional (fname *dic-ex*))
  "fname ファイルにセーブした n-gram を *n-gram-ex* に読み込む。
fname を省略すると *dic-ex* から読み込む。"
  (setf *n-gram-ex* nil)
  (with-open-file (in fname)
    (loop
       :for line = (read in nil)
       :while line
       :do (setf *n-gram-ex* (nconc *n-gram-ex* line)))))

(defun load-dic-ex-1 (&optional (fname *dic-ex*))
  "fname ファイルにセーブした n-gram を *n-gram-ex* に読み込む。
fname を省略すると *dic-ex* から読み込む。nconc の順番を変えてみる。"
  (setf *n-gram-ex* nil)
  (with-open-file (in fname)
    (loop
       :for line = (read in nil)
       :while line
       :do (setf *n-gram-ex* (nconc line *n-gram-ex*)))))

;; 処理時間に差はない。データが小さいか。
(time (load-dic-ex-0))
(time (load-dic-ex-1))

;;ファイルを文字列に読む。
(defun read-file (fname)
  (with-open-file (in fname)
    (let* ((len (file-length in))
           (buf (make-string len)))
      (format t "length: ~d~%" len)
      (read-sequence buf in)
      buf)))

;; alexandria を利用する。
(ql:quickload :alexandria)
(alexandria:read-file-into-string "sample.lisp")

;;ファイルの一行を要素として持つリストを返す。
;;do マクロ
(defun read-file-into-list-using-do (fname)
  (let ((ret nil))
    (with-open-file (in fname)
      (do ((line (read-line in nil)))
          ((null line) (nreverse ret))
        (push line ret)
        (setf line (read-line in nil))))))

;;loopマクロ。キーワードをシンボルで書いたほうが読みやすい？
(defun read-file-into-list-using-loop (fname)
  (let ((ret nil))
    (with-open-file (in fname)
      (loop
         :for line = (read-line in nil)
         :while line
         :do (push line ret)))
    (nreverse ret)))
