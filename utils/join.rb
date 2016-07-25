#!/usr/bin/env ruby
# coding: utf-8
# 行が句点（。）で終わるようにする。
# どこで使う？

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
