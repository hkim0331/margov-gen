#!/usr/bin/env ruby
# coding: utf-8
# 行が読点で終わるようにする。

buf = ""
while (line = STDIN.gets)
  line = line.strip
  if line =~ /。$/
    puts buf + line
    buf = ""
  else
    buf << line
  end
end
