.SUFFIXES: .html .txt .mecab

.html.txt:
	nkf -w $< | ruby extract.rb | \
		sed 's/<ruby>.*<\/ruby>//g' | \
		ruby join.rb > $@

.txt.mecab:
	mecab $< -O wakati > $@

all: 春.mecab 最後の一枚の葉.mecab 賢者の贈り物.mecab

clean:
	${RM} *~ *.txt *.mecab





