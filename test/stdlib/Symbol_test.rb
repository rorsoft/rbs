require_relative "test_helper"

class SymbolTest < StdlibTest
  target Symbol
  using hook.refinement

  def test_all_symbols
    Symbol.all_symbols
  end

  def test_cmp
    :a <=> :a
    :a <=> :b
    :b <=> :a
    :a <=> 42
  end

  def test_eq
    :a == :a
    :a == 42
  end

  def test_eqq
    :a === :a
    :a === 42
  end

  def test_match
    :a =~ /a/
    :a =~ nil
  end

  def test_aref
    :a[0] == "a" or raise
    :a[1] == nil or raise
    :a[0, 1] == "a" or raise
    :a[2, 1] == nil or raise
    :a[0..1] == "a" or raise
    :a[2..1] == nil or raise
    :a[0...] == "a" or raise
    :a[2...] == nil or raise
    if RUBY_27_OR_LATER
      eval(<<~RUBY)
        :a[...0] == "" or raise
      RUBY
    end
    :a[/a/] == "a" or raise
    :a[/b/] == nil or raise
    :a[/a/, 0] == "a" or raise
    :a[/b/, 0] == nil or raise
    :a[/(?<a>a)/, "a"] == "a" or raise
    :a[/(?<b>b)/, "b"] == nil or raise
    :a["a"] == "a" or raise
    :a["b"] == nil or raise
  end

  def test_capitalize
    :a.capitalize
    :a.capitalize(:ascii)
    :a.capitalize(:lithuanian)
    :a.capitalize(:turkic)
    :a.capitalize(:lithuanian, :turkic)
    :a.capitalize(:turkic, :lithuanian)
  end

  def test_casecmp
    :a.casecmp(:A)
    :a.casecmp(:B)
    :b.casecmp(:A)
    "\u{e4 f6 fc}".encode("ISO-8859-1").to_sym.casecmp(:"\u{c4 d6 dc}")
    :a.casecmp(42)
  end

  def test_casecmp_p
    :a.casecmp(:A)
    :a.casecmp(:B)
    "\u{e4 f6 fc}".encode("ISO-8859-1").to_sym.casecmp(:"\u{c4 d6 dc}")
    :a.casecmp(42)
  end

  def test_downcase
    :a.downcase
    :a.downcase(:ascii)
    :a.downcase(:fold)
    :a.downcase(:lithuanian)
    :a.downcase(:turkic)
    :a.downcase(:lithuanian, :turkic)
    :a.downcase(:turkic, :lithuanian)
  end

  def test_empty_p
    :"".empty?
    :a.empty?
  end

  def test_encoding
    :a.encoding
  end

  if RUBY_27_OR_LATER
    def test_end_with?
      :a.end_with?("a")
      :a.end_with?("b")
      :a.end_with?("a", "b")
    end
  end

  if RUBY_27_OR_LATER
    def test_start_with?
      :a.start_with?("a")
      :a.start_with?("b")
    end
  end
end
