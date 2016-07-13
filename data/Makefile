#
# CAUTION: LANG=ja_JP.UTF-8
#
.SUFFIXES: .html .txt .mecab

.html.txt:
	LANG=ja_JP.UTF-8 nkf -w $< | ruby extract.rb | \
		sed 's/<ruby>.*<\/ruby>//g' | \
		ruby join.rb > $@

.txt.mecab:
	LANG=ja_JP.UTF-8 mecab $< -O wakati > $@

all: 春.mecab 最後の一枚の葉.mecab 賢者の贈り物.mecab 坊っちゃん.mecab

clean:
	${RM} *~ *.mecab





