require "test_helper"
require "rbs/test"

class SetupHelperTest < Minitest::Test
  include RBS::Test::SetupHelper

  def test_get_valid_sample_size
    assert_equal 100, get_sample_size("100")
    assert_nil get_sample_size("ALL")

    Array.new(1000) { |int| int.succ.to_s}.each do
      |str| assert_equal str.to_i, get_sample_size(str)
    end
  end

  def test_get_invalid_sample_size_error
    assert_raises_invalid_sample_size_error("yes")
    assert_raises_invalid_sample_size_error("0")
    assert_raises_invalid_sample_size_error("-1")
    assert_raises_invalid_sample_size_error(nil)
  end

  def assert_raises_invalid_sample_size_error(invalid_value)
    assert_raises InvalidSampleSizeError do
      get_sample_size(invalid_value)
    end    
  end
end

