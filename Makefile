magi.wakati: data/賢者の贈り物.txt
	mecab $< -O wakati > $@

sample.wakati: sample.txt
	mecab $< -O wakati > $@
