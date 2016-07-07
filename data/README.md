青空文庫より。

## 文字コード

```
s/encoding="Shift_JIS"/encoding="UTF-8"/
s/charset=Shift_JIS/charset=UTF-8/
```

C-x RET f utf-8

## ルビを外す

ルビが振られていて邪魔。

Makefile:

```
extract: 春.html 最後の一枚の葉.html 賢者の贈り物.html
	for i in $^; do \
		nkf -w $$i | ruby extract.rb | \
			sed 's/<ruby>.*<\/ruby>//g' > `basename $$i .html`.txt; \
	done
```

## 読点までの文にする


