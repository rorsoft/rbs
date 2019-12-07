#!/usr/bin/env ruby

args = if ARGV.empty?
         Pathname.glob("#{__dir__}/../test/stdlib/**/*_test.rb")
       else
         ARGV
       end

args.each do |arg|
  load arg
end
