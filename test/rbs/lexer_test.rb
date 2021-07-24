require "test_helper"

class RBS::LexerTest < Test::Unit::TestCase
  include TestHelper
  include RBS

  class Lex < Lexer
    skip(/\s+/)
    token(:PLUS, /\+/)
    token(:MINUS, /\-/)
    token(:VAR, /[a-z]+/)
    token_invoke(:number, /\d+/)

    def number(string)
      yield :NUMBER, string.to_i
    end
  end

  def test_foo
    lex = Lex.new("1 + 2 - abc")

    lex.next_token.tap do |tok, val|
      assert_equal :NUMBER, tok
      assert_equal 1, lex.value(val)
      assert_equal 0, lex.start_pos(val)
      assert_equal 0, lex.end_pos(val)
    end

    lex.next_token.tap do |tok, val|
      assert_equal :PLUS, tok
      assert_equal "+", lex.value(val)
      assert_equal 2, lex.start_pos(val)
      assert_equal 2, lex.end_pos(val)
    end

    lex.next_token.tap do |tok, val|
      assert_equal :NUMBER, tok
      assert_equal 2, lex.value(val)
      assert_equal 4, lex.start_pos(val)
      assert_equal 4, lex.end_pos(val)
    end

    lex.next_token.tap do |tok, val|
      assert_equal :MINUS, tok
      assert_equal "-", lex.value(val)
      assert_equal 6, lex.start_pos(val)
      assert_equal 6, lex.end_pos(val)
    end

    lex.next_token.tap do |tok, val|
      assert_equal :VAR, tok
      assert_equal "abc", lex.value(val)
      assert_equal 8, lex.start_pos(val)
      assert_equal 10, lex.end_pos(val)
    end

    assert_nil lex.next_token
  end
end
