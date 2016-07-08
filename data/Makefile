.SUFFIXES: .html .txt .wakati

.html.txt:
	nkf -w $< | ruby extract.rb | \
		sed 's/<ruby>.*<\/ruby>//g' | \
		ruby join.rb > $@

.txt.wakati:
	mecab $< -O wakati > $@

all: 春.wakati 最後の一枚の葉.wakati 賢者の贈り物.wakati

clean:
	${RM} *~ *.txt *.wakati




