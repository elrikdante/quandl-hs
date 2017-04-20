#!/usr/bin/env ruby
require 'ascii_charts'
require 'json'
data = JSON(STDIN.read.chomp)
STDOUT.puts(AsciiCharts::Cartesian.new(data["dataset"]["data"].first(10)).draw)
