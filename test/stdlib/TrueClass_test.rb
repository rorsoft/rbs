require_relative "test_helper"

class TrueClassTest < StdlibTest
  target TrueClass
  using hook.refinement

  def test_eqq
    true === true
    true === false
  end
end
