class EnumerableTest < StdlibTest
  target Enumerable
  using hook.refinement

  def test_find_all
    enumerable.find_all
    enumerable.find_all { |x| x.even? }
  end

  def test_filter
    enumerable.filter
    enumerable.filter { |x| x.even? }
  end

  def test_grep
    enumerable.grep(-> x { x.even? })
    enumerable.grep(-> x { x.even? }) { |x| x * 2 }
  end

  def test_grep_v
    enumerable.grep_v(-> x { x.even? })
    enumerable.grep_v(-> x { x.even? }) { |x| x * 2 }
  end

  def test_select
    enumerable.select
    enumerable.select { |x| x.even? }
  end

  def test_uniq
    enumerable.uniq
    enumerable.uniq { |x| x.even? }
  end

  def test_sum
    enumerable.sum
    enumerable.sum { |x| x * 2 }
    enumerable.sum(0)
    enumerable.sum('') { |x| x.to_s }
  end

  def test_filter_map
    enumerable.filter_map
    enumerable.filter_map { |x| x.even? && x * 2 }
  end

  def test_chain
    enumerable.chain
    enumerable.chain([4, 5])
  end

  def test_tally
    enumerable.tally
  end

  def test_each_entry
    enumerable.each_entry
    enumerable.each_entry { |x| x }
  end

  def test_zip
    enumerable.zip([4,5,6])
    enumerable.zip([4,5,6]) { |arr| arr.sum }
  end

  def test_chunk
    enumerable.chunk
    enumerable.chunk { |x| x.even? }
  end

  private

  def enumerable
    Class.new {
      def each
        yield 1
        yield 2
        yield 3
      end

      include Enumerable
    }.new
  end
end
