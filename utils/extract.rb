#!/usr/bin/env ruby
# coding: utf-8
# 入力は UTF-8 の文字列。
# 会話の部分「...」だけを抜き出す。

inword = false

while (line = STDIN.gets)
  line.chars.each do |char|
    if inword
      if char == "」"
        puts "。"
        inword = false
      else
        print char
      end
    else
      if char == "「"
        inword = true
      end
    end
  end
end
