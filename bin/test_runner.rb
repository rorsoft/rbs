#!/usr/bin/env ruby

RUBY_27 = Gem::Version.new(RUBY_VERSION).yield_self do |ruby_version|
  Gem::Version.new('2.7.0') <= ruby_version && ruby_version < Gem::Version.new("2.8.0")
end

unless RUBY_27
  STDERR.puts "🚨🚨🚨 stdlib test requires Ruby 2.7 but RUBY_VERSION==#{RUBY_VERSION}, exiting... 🚨🚨🚨"
  exit
end

ARGV.each do |arg|
  load arg
end
