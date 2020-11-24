require "test_helper"
require "rbs/test"
require "logger"

return unless Gem::Version.new(RUBY_VERSION) >= Gem::Version.new('2.7.0')

class RBS::Test::RuntimeTestTest < Minitest::Test
  include TestHelper

  def test_runtime_success
    output = assert_test_success()
    assert_match "Setting up hooks for ::Hello", output
    refute_match "No type checker was installed!", output
  end

  def test_runtime_test_with_sample_size
    assert_test_success(other_env: {"RBS_TEST_SAMPLE_SIZE" => '30'})
    assert_test_success(other_env: {"RBS_TEST_SAMPLE_SIZE" => '100'})
    assert_test_success(other_env: {"RBS_TEST_SAMPLE_SIZE" => 'ALL'})
  end

  def test_runtime_test_error_with_invalid_sample_size
    string_err_msg = refute_test_success(other_env: {"RBS_TEST_SAMPLE_SIZE" => 'yes'})
    assert_match(/E, .+ ERROR -- rbs: Sample size should be a positive integer: `.+`\n/, string_err_msg)

    zero_err_msg = refute_test_success(other_env: {"RBS_TEST_SAMPLE_SIZE" => '0'})
    assert_match(/E, .+ ERROR -- rbs: Sample size should be a positive integer: `.+`\n/, zero_err_msg)

    negative_err_msg = refute_test_success(other_env: {"RBS_TEST_SAMPLE_SIZE" => '-1'})
    assert_match(/E, .+ ERROR -- rbs: Sample size should be a positive integer: `.+`\n/, negative_err_msg)
  end

  def run_runtime_test(other_env:, rbs_content: nil, ruby_content: nil)
    SignatureManager.new(system_builtin: true) do |manager|
      manager.files[Pathname("foo.rbs")] = rbs_content || <<EOF
class Hello
  attr_reader x: Integer
  attr_reader y: Integer

  def initialize: (x: Integer, y: Integer) -> void

  def move: (?x: Integer, ?y: Integer) -> void
end
EOF

      manager.build do |env, path|
        (path + "sample.rb").write(ruby_content || <<RUBY)
class Hello
  attr_reader :x, :y

  def initialize(x:, y:)
    @x = x
    @y = y
  end

  def move(x: 0, y: 0)
    @x += x
    @y += y
  end
end

hello = Hello.new(x: 0, y: 10)
RUBY

        env = {
          "BUNDLE_GEMFILE" => File.join(__dir__, "../../../Gemfile"),
          "RBS_TEST_TARGET" => "::Hello",
          "RBS_TEST_OPT" => "-I./foo.rbs"
        }
        ruby = ENV['RUBY'] || RbConfig.ruby
        command_line = if defined?(Bundler)
                         [ruby, "-rbundler/setup", "-rrbs/test/setup", "sample.rb"]
                       else
                         [ruby, "-I#{__dir__}/../../../lib", "-EUTF-8", "-rrbs/test/setup", "sample.rb"]
                       end

        _out, err, status = Open3.capture3(env.merge(other_env), *command_line, chdir: path.to_s)

        return [err, status]
      end
    end
  end

  def assert_test_success(other_env: {}, rbs_content: nil, ruby_content: nil)
    err, status = run_runtime_test(other_env: other_env, rbs_content: rbs_content, ruby_content: ruby_content)
    assert_predicate status, :success?, err
    err
  end

  def refute_test_success(other_env: {}, rbs_content: nil, ruby_content: nil)
    err, status = run_runtime_test(other_env: other_env, rbs_content: rbs_content, ruby_content: ruby_content)
    refute_predicate status, :success?, err
    err
  end

  def test_test_target
    output = refute_test_success(other_env: { "RBS_TEST_TARGET" => nil })
    assert_match "rbs/test/setup handles the following environment variables:", output
  end

  def test_no_test_install
    output = assert_test_success(other_env: { "RBS_TEST_TARGET" => "NO_SUCH_CLASS" })
    refute_match "Setting up hooks for ::Hello", output
    assert_match "No type checker was installed!", output
  end

  def test_name_override
    output = assert_test_success(ruby_content: <<RUBY)
class TestClass
  def self.name
    raise
  end
end
RUBY
    assert_match "No type checker was installed!", output
  end

  def test_open_decls
    output = refute_test_success(ruby_content: <<RUBY, rbs_content: <<RBS)
class Hello
end

class Hello
  def world
  end
end

Hello.new.world(3)
RUBY
class Hello
  def world: () -> void
end
RBS

    assert_match(/TypeError: \[Hello#world\]/, output)
  end

  def test_minitest
    skip unless has_gem?("minitest")

    assert_test_success(other_env: { 'RBS_TEST_TARGET' => 'Foo', 'RBS_TEST_DOUBLE_SUITE' => 'minitest' }, rbs_content: <<RBS, ruby_content: <<RUBY)
class Foo
  def foo: (Integer) -> void
end
RBS

class Foo
  def foo(integer)
  end
end

require "minitest/autorun"

class FooTest < Minitest::Test
  def test_foo_mock
    # Confirm if mock is correctly ignored.
    Foo.new.foo(::Minitest::Mock.new)
  end

  def test_no_foo
    # Confirm if RBS runtime test raises errors when unexpected object is given.
    assert_raises RBS::Test::Tester::TypeError do
      Foo.new.foo("")
    end
  end
end
RUBY
  end

  def test_rspec
    skip unless has_gem?("rspec")

    assert_test_success(other_env: { "RBS_TEST_TARGET" => 'Foo', "RBS_TEST_DOUBLE_SUITE" => 'rspec' }, rbs_content: <<RBS, ruby_content: <<RUBY)
class Foo
  def foo: (Integer) -> void
end
RBS

class Foo
  def foo(integer)
  end
end

require 'rspec'

RSpec::Core::Runner.autorun

describe 'Foo' do
  describe "#foo" do
    it "accepts doubles" do
      # Confirm if double is correctly ignored.
      Foo.new.foo(double('foo'))
    end

    it "does not accept non_integers" do
      # Confirm if RBS runtime test raises errors when unexpected object is given.
      expect { Foo.new.foo("") }.to raise_error(RBS::Test::Tester::TypeError)
    end
  end
end
RUBY
  end

  def test_instance_eval
    assert_test_success(other_env: { 'RBS_TEST_TARGET' => 'Foo' }, rbs_content: <<RBS, ruby_content: <<RUBY)
class Foo
  def foo: (Integer) { (Integer) -> Integer } -> Integer
end
RBS

class Foo
  def foo(integer, &block)
    integer.instance_eval(&block)
  end
end

Foo.new.foo(10) do
  self + 3
end
RUBY
  end

  def test_super
    assert_test_success(other_env: { 'RBS_TEST_TARGET' => 'Foo,Bar' }, rbs_content: <<RBS, ruby_content: <<'RUBY')
class Foo
  def foo: (Integer) -> void
end

class Bar
  def foo: (Integer) -> void
end
RBS

class Foo
  def foo(x)
    ::RBS.logger.error("Foo#foo")
    puts "Foo#foo: x=#{x}"
  end
end

class Bar < Foo
  def foo(x)
    ::RBS.logger.error("Bar#foo")
    puts "Bar#foo: x=#{x}"
    super
  end
end

Bar.new.foo(30)
RUBY
  end
end
