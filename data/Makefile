all: wakati

extract: 春.html 最後の一枚の葉.html 賢者の贈り物.html
	for i in $^; do \
		nkf -w $$i | ruby extract.rb | \
		sed 's/<ruby>.*<\/ruby>//g' | \
		ruby join.rb > `basename $$i .html`.txt; \
	done

wakati: extract
	for i in *.txt; do \
		mecab $$i -O wakati > `basename $$i .txt`.wakati ; \
	done

clean:
	${RM} *.txt


