require "test_helper"

class Ruby::Signature::SignatureParsingTest < Minitest::Test
  Parser = Ruby::Signature::Parser
  Buffer = Ruby::Signature::Buffer
  Types = Ruby::Signature::Types
  TypeName = Ruby::Signature::TypeName
  Namespace = Ruby::Signature::Namespace
  Declarations = Ruby::Signature::AST::Declarations
  Members = Ruby::Signature::AST::Members
  MethodType = Ruby::Signature::MethodType

  include TestHelper

  def test_type_alias
    Parser.parse_signature("type foo = any").yield_self do |decls|
      assert_equal 1, decls.size

      type_decl = decls[0]

      assert_instance_of Declarations::Alias, type_decl
      assert_equal :foo, type_decl.name
      assert_equal Types::Bases::Any.new(location: nil), type_decl.type
      assert_equal "type foo = any", type_decl.location.source
    end
  end

  def test_constant
    Parser.parse_signature("FOO: any").yield_self do |decls|
      assert_equal 1, decls.size

      const_decl = decls[0]

      assert_instance_of Declarations::Constant, const_decl
      assert_equal TypeName.new(name: :FOO, namespace: Namespace.empty), const_decl.name
      assert_equal Types::Bases::Any.new(location: nil), const_decl.type
      assert_equal "FOO: any", const_decl.location.source
    end

    Parser.parse_signature("::BAR: any").yield_self do |decls|
      assert_equal 1, decls.size

      const_decl = decls[0]

      assert_instance_of Declarations::Constant, const_decl
      assert_equal TypeName.new(name: :BAR, namespace: Namespace.root), const_decl.name
      assert_equal Types::Bases::Any.new(location: nil), const_decl.type
      assert_equal "::BAR: any", const_decl.location.source
    end

    Parser.parse_signature("FOO : any").yield_self do |decls|
      assert_equal 1, decls.size

      const_decl = decls[0]

      assert_instance_of Declarations::Constant, const_decl
      assert_equal TypeName.new(name: :FOO, namespace: Namespace.empty), const_decl.name
      assert_equal Types::Bases::Any.new(location: nil), const_decl.type
      assert_equal "FOO : any", const_decl.location.source
    end

    Parser.parse_signature("::BAR : any").yield_self do |decls|
      assert_equal 1, decls.size

      const_decl = decls[0]

      assert_instance_of Declarations::Constant, const_decl
      assert_equal TypeName.new(name: :BAR, namespace: Namespace.root), const_decl.name
      assert_equal Types::Bases::Any.new(location: nil), const_decl.type
      assert_equal "::BAR : any", const_decl.location.source
    end
  end

  def test_global
    Parser.parse_signature("$FOO: any").yield_self do |decls|
      assert_equal 1, decls.size

      global_decl = decls[0]

      assert_instance_of Declarations::Global, global_decl
      assert_equal :"$FOO", global_decl.name
      assert_equal Types::Bases::Any.new(location: nil), global_decl.type
      assert_equal "$FOO: any", global_decl.location.source
    end
  end

  def test_interface
    Parser.parse_signature("interface _Each[A, B] end").yield_self do |decls|
      assert_equal 1, decls.size

      interface_decl = decls[0]

      assert_instance_of Declarations::Interface, interface_decl
      assert_equal TypeName.new(name: :_Each, namespace: Namespace.empty), interface_decl.name
      assert_equal [:A, :B], interface_decl.type_params
      assert_equal [], interface_decl.members
      assert_equal "interface _Each[A, B] end", interface_decl.location.source
    end

    Parser.parse_signature(<<~SIG).yield_self do |decls|
        interface _Each[A, B]
          #
          # Yield all elements included in `self`.
          #  
          def count: -> Integer
                   | (any) -> Integer
                   | [X] { (A) -> X } -> Integer

          include _Hash[Integer]
        end
      SIG

      assert_equal 1, decls.size

      interface_decl = decls[0]

      assert_instance_of Declarations::Interface, interface_decl
      assert_equal TypeName.new(name: :_Each, namespace: Namespace.empty), interface_decl.name
      assert_equal [:A, :B], interface_decl.type_params

      assert_equal 2, interface_decl.members.size
      interface_decl.members[0].yield_self do |def_member|
        assert_instance_of Members::MethodDefinition, def_member
        assert_equal :count, def_member.name
        assert_equal 3, def_member.types.size

        def_member.types.yield_self do |t1, t2, t3|
          assert_empty t1.type_params
          assert_nil t1.block
          assert_equal "-> Integer", t1.location.source

          assert_empty t2.type_params
          assert_equal 1, t2.type.required_positionals.size
          assert_nil t2.block
          assert_equal "(any) -> Integer", t2.location.source

          assert_equal [:X], t3.type_params
          assert_instance_of MethodType::Block, t3.block
          assert_instance_of Types::Variable, t3.block.type.required_positionals[0].type
          assert_instance_of Types::Variable, t3.block.type.return_type
          assert_equal "[X] { (A) -> X } -> Integer", t3.location.source
        end
      end

      interface_decl.members[1].yield_self do |include_member|
        assert_instance_of Members::Include, include_member
        assert_equal TypeName.new(name: :_Hash, namespace: Namespace.empty), include_member.name
        assert_equal [parse_type("Integer")], include_member.args
      end
    end

    assert_raises Parser::SemanticsError do
      Parser.parse_signature(<<~SIG)
        interface _Each[A, B]
          def self.foo: -> void
        end
      SIG
    end

    assert_raises Parser::SemanticsError do
      Parser.parse_signature(<<~SIG)
        interface _Each[A, B]
          include Object
        end
      SIG
    end
  end

  def test_module
    Parser.parse_signature("module Enumerable[A, B] : _Each end").yield_self do |decls|
      assert_equal 1, decls.size

      module_decl = decls[0]

      assert_instance_of Declarations::Module, module_decl
      assert_equal TypeName.new(name: :Enumerable, namespace: Namespace.empty), module_decl.name
      assert_equal [:A, :B], module_decl.type_params
      assert_equal parse_type("_Each"), module_decl.self_type
      assert_equal [], module_decl.members
      assert_equal "module Enumerable[A, B] : _Each end", module_decl.location.source
    end

    Parser.parse_signature(<<~SIG).yield_self do |decls|
        module Enumerable[A, B] : _Each
          @foo: String
          self.@bar: Integer
          @@baz: Array[Integer]

          def one: -> void
          def self.two: -> any
          def self?.three: -> bool

          include X
          include _X

          extend Y
          extend _Y

          attr_reader a: Integer
          attr_reader a(@A): String
          attr_reader a(): bool

          attr_writer b: Integer
          attr_writer b(@B): String
          attr_writer b(): bool

          attr_accessor c: Integer
          attr_accessor c(@C): String
          attr_accessor c(): bool
        end
      SIG

      assert_equal 1, decls.size

      module_decl = decls[0]

      assert_instance_of Declarations::Module, module_decl

      assert_equal 19, module_decl.members.size

      module_decl.members[0].yield_self do |m|
        assert_instance_of Members::InstanceVariable, m
        assert_equal :@foo, m.name
        assert_equal parse_type("String"), m.type
      end

      module_decl.members[1].yield_self do |m|
        assert_instance_of Members::ClassInstanceVariable, m
        assert_equal :@bar, m.name
        assert_equal parse_type("Integer"), m.type
      end

      module_decl.members[2].yield_self do |m|
        assert_instance_of Members::ClassVariable, m
        assert_equal :@@baz, m.name
        assert_equal parse_type("Array[Integer]"), m.type
      end

      module_decl.members[3].yield_self do |m|
        assert_instance_of Members::MethodDefinition, m
        assert_equal :instance, m.kind
        assert_equal "def one: -> void", m.location.source
      end

      module_decl.members[4].yield_self do |m|
        assert_instance_of Members::MethodDefinition, m
        assert_equal :singleton, m.kind
        assert_equal "def self.two: -> any", m.location.source
      end

      module_decl.members[5].yield_self do |m|
        assert_instance_of Members::MethodDefinition, m
        assert_equal :singleton_instance, m.kind
        assert_equal "def self?.three: -> bool", m.location.source
      end

      module_decl.members[6].yield_self do |m|
        assert_instance_of Members::Include, m
        assert_equal TypeName.new(name: :X, namespace: Namespace.empty), m.name
        assert_equal [], m.args
        assert_equal "include X", m.location.source
      end

      module_decl.members[7].yield_self do |m|
        assert_instance_of Members::Include, m
        assert_equal TypeName.new(name: :_X, namespace: Namespace.empty), m.name
        assert_equal [], m.args
        assert_equal "include _X", m.location.source
      end

      module_decl.members[8].yield_self do |m|
        assert_equal "extend Y", m.location.source
        assert_instance_of Members::Extend, m
        assert_equal TypeName.new(name: :Y, namespace: Namespace.empty), m.name
        assert_equal [], m.args
      end

      module_decl.members[9].yield_self do |m|
        assert_equal "extend _Y", m.location.source
        assert_instance_of Members::Extend, m
        assert_equal TypeName.new(name: :_Y, namespace: Namespace.empty), m.name
        assert_equal [], m.args
      end

      module_decl.members[10].yield_self do |m|
        assert_equal "attr_reader a: Integer", m.location.source
        assert_instance_of Members::AttrReader, m
        assert_equal :a, m.name
        assert_nil m.ivar_name
        assert_equal parse_type("Integer"), m.type
      end

      module_decl.members[11].yield_self do |m|
        assert_equal "attr_reader a(@A): String", m.location.source
        assert_instance_of Members::AttrReader, m
        assert_equal :a, m.name
        assert_equal :@A, m.ivar_name
        assert_equal parse_type("String"), m.type
      end

      module_decl.members[12].yield_self do |m|
        assert_equal "attr_reader a(): bool", m.location.source
        assert_instance_of Members::AttrReader, m
        assert_equal :a, m.name
        assert_equal false, m.ivar_name
        assert_equal parse_type("bool"), m.type
      end

      module_decl.members[13].yield_self do |m|
        assert_equal "attr_writer b: Integer", m.location.source
        assert_instance_of Members::AttrWriter, m
        assert_equal :b, m.name
        assert_nil m.ivar_name
        assert_equal parse_type("Integer"), m.type
      end

      module_decl.members[14].yield_self do |m|
        assert_equal "attr_writer b(@B): String", m.location.source
        assert_instance_of Members::AttrWriter, m
        assert_equal :b, m.name
        assert_equal :@B, m.ivar_name
        assert_equal parse_type("String"), m.type
      end

      module_decl.members[15].yield_self do |m|
        assert_equal "attr_writer b(): bool", m.location.source
        assert_instance_of Members::AttrWriter, m
        assert_equal :b, m.name
        assert_equal false, m.ivar_name
        assert_equal parse_type("bool"), m.type
      end

      module_decl.members[16].yield_self do |m|
        assert_equal "attr_accessor c: Integer", m.location.source
        assert_instance_of Members::AttrAccessor, m
        assert_equal :c, m.name
        assert_nil m.ivar_name
        assert_equal parse_type("Integer"), m.type
      end

      module_decl.members[17].yield_self do |m|
        assert_equal "attr_accessor c(@C): String", m.location.source
        assert_instance_of Members::AttrAccessor, m
        assert_equal :c, m.name
        assert_equal :@C, m.ivar_name
        assert_equal parse_type("String"), m.type
      end

      module_decl.members[18].yield_self do |m|
        assert_equal "attr_accessor c(): bool", m.location.source
        assert_instance_of Members::AttrAccessor, m
        assert_equal :c, m.name
        assert_equal false, m.ivar_name
        assert_equal parse_type("bool"), m.type
      end
    end
  end

  def test_class
    Parser.parse_signature("class Array[A] end").yield_self do |decls|
      assert_equal 1, decls.size

      decls[0].yield_self do |class_decl|
        assert_instance_of Declarations::Class, class_decl
        assert_equal TypeName.new(name: :Array, namespace: Namespace.empty), class_decl.name
        assert_equal [:A], class_decl.type_params
        assert_nil class_decl.super_class
      end
    end

    Parser.parse_signature("class ::Array[A] < Object[A] end").yield_self do |decls|
      assert_equal 1, decls.size

      decls[0].yield_self do |class_decl|
        assert_instance_of Declarations::Class, class_decl
        assert_equal TypeName.new(name: :Array, namespace: Namespace.root), class_decl.name
        assert_equal [:A], class_decl.type_params

        assert_instance_of Declarations::Class::Super, class_decl.super_class
        assert_equal TypeName.new(name: :Object, namespace: Namespace.empty), class_decl.super_class.name
        assert_equal [parse_type("A", variables: [:A])], class_decl.super_class.args
      end
    end
  end

  def test_method_definition
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      class Foo[X, Y]
        def foo: -> Integer
               | ?{ -> void } -> Integer
               | [A] () { (String, ?Object, *Float, Symbol, foo: bool, ?bar: any, **Y) -> X } -> A
      end
    SIG
      assert_equal 1, decls.size

      decls[0].yield_self do |decl|
        assert_instance_of Declarations::Class, decl

        assert_instance_of Members::MethodDefinition, decl.members[0]
        decl.members[0].yield_self do |m|
          assert_equal 3, m.types.size

          m.types[0].yield_self do |ty|
            assert_instance_of MethodType, ty
            assert_equal "-> Integer", ty.location.source
            assert_nil ty.block
          end

          m.types[1].yield_self do |ty|
            assert_instance_of MethodType, ty
            assert_equal "?{ -> void } -> Integer", ty.location.source
            assert_instance_of MethodType::Block, ty.block
            refute ty.block.required
          end

          m.types[2].yield_self do |ty|
            assert_instance_of MethodType, ty
            assert_equal "[A] () { (String, ?Object, *Float, Symbol, foo: bool, ?bar: any, **Y) -> X } -> A", ty.location.source
            assert_instance_of MethodType::Block, ty.block
            assert ty.block.required
          end
        end
      end
    end
  end

  def test_method_super
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      class Foo
        def foo: -> void
               | super
      end
    SIG
      type1, type2 = decls[0].members[0].types

      assert_instance_of MethodType, type1
      assert_equal "-> void", type1.location.source

      assert_equal :super, type2
    end
  end

  def test_private_public
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      class Foo
        public
        private
      end
    SIG

      decls[0].yield_self do |decl|
        assert_instance_of Declarations::Class, decl

        assert_equal 2, decl.members.size

        decl.members[0].yield_self do |m|
          assert_instance_of Members::Public, m
        end

        decl.members[1].yield_self do |m|
          assert_instance_of Members::Private, m
        end
      end
    end
  end

  def test_alias
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      class Foo
        def foo: -> String
        alias bar foo
        alias self.bar self.foo
      end
    SIG

      decls[0].yield_self do |decl|
        assert_equal 3, decl.members.size

        decl.members[1].yield_self do |m|
          assert_instance_of Members::Alias, m
          assert_equal :bar, m.new_name
          assert_equal :foo, m.old_name
          assert_equal :instance, m.kind
          assert_equal "alias bar foo", m.location.source
        end

        decl.members[2].yield_self do |m|
          assert_instance_of Members::Alias, m
          assert_equal :bar, m.new_name
          assert_equal :foo, m.old_name
          assert_equal :singleton, m.kind
          assert_equal "alias self.bar self.foo", m.location.source
        end
      end
    end
  end

  def test_method_names
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      interface _Foo
        def class: -> String
        def void: -> String
        def nil?: -> String
        def any: -> String
        def top: -> String
        def bot: -> String
        def `instance`: -> String
        def `self?`: -> String
        def bool: -> String
        def singleton: -> String
        def type: -> String
        def module: -> String
        def private: -> String
        def public: -> any
        def &: (any) -> any
        def ^: (any) -> any
        def *: (any) -> any
        def ==: (any) -> any
        def <: (any) -> any
        def <=: (any) -> any
        def >: (any) -> any
        def >=: (any) -> any
        def end: -> any
        def include: -> any
        def extend: -> any
        def attr_reader: -> any
        def attr_accessor: -> any
        def attr_writer: -> any
        def `\\``: -> any
        def def!: -> any
        def !: -> any
      end
    SIG
      expected_names = [
        :class,
        :void,
        :nil?,
        :any,
        :top,
        :bot,
        :instance,
        :self?,
        :bool,
        :singleton,
        :type,
        :module,
        :private,
        :public,
        :&,
        :^,
        :*,
        :==,
        :<,
        :<=,
        :>,
        :>=,
        :end,
        :include,
        :extend,
        :attr_reader,
        :attr_accessor,
        :attr_writer,
        :`,
        :def!,
        :!
      ]

      assert_equal expected_names, decls[0].members.map(&:name)
    end
  end

  def test_annotation_on_declaration
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      %a(foo)
      %a[hello world]
      class Hello end
      
      %a{Lorem Ipsum}
      module Foo end

      %a< undocumented >
      interface _Foo end

      %a|bar is okay|
      type string = String
    SIG

      decls[0].yield_self do |decl|
        assert_instance_of Declarations::Class, decl

        assert_equal "foo", decl.annotations[0].string
        assert_equal "%a(foo)", decl.annotations[0].location.source

        assert_equal "hello world", decl.annotations[1].string
        assert_equal "%a[hello world]", decl.annotations[1].location.source
      end

      decls[1].yield_self do |decl|
        assert_instance_of Declarations::Module, decl

        assert_equal "Lorem Ipsum", decl.annotations[0].string
        assert_equal "%a{Lorem Ipsum}", decl.annotations[0].location.source
      end

      decls[2].yield_self do |decl|
        assert_instance_of Declarations::Interface, decl

        assert_equal "undocumented", decl.annotations[0].string
        assert_equal "%a< undocumented >", decl.annotations[0].location.source
      end

      decls[3].yield_self do |decl|
        assert_instance_of Declarations::Alias, decl

        assert_equal "bar is okay", decl.annotations[0].string
        assert_equal "%a|bar is okay|", decl.annotations[0].location.source
      end
    end
  end

  def test_annotations_on_members
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      class Hello
        %a{noreturn}
        def foo: () -> any

        %a[incompatible]
        include Foo

        %a{prepend}
        extend Foo

        %a{dynamic}
        attr_reader foo: Bar

        %a{constructor}
        alias to_str to_s
      end
    SIG

      decls[0].members[0].yield_self do |m|
        assert_instance_of Members::MethodDefinition, m
        assert_equal ["noreturn"], m.annotations.map(&:string)
      end

      decls[0].members[1].yield_self do |m|
        assert_instance_of Members::Include, m
        assert_equal ["incompatible"], m.annotations.map(&:string)
      end

      decls[0].members[2].yield_self do |m|
        assert_instance_of Members::Extend, m
        assert_equal ["prepend"], m.annotations.map(&:string)
      end

      decls[0].members[3].yield_self do |m|
        assert_instance_of Members::AttrReader, m
        assert_equal ["dynamic"], m.annotations.map(&:string)
      end

      decls[0].members[4].yield_self do |m|
        assert_instance_of Members::Alias, m
        assert_equal ["constructor"], m.annotations.map(&:string)
      end
    end
  end

  def test_prepend
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      class Foo
        prepend Foo
      end
    SIG

      decls[0].members[0].yield_self do |m|
        assert_instance_of Members::Prepend, m
        assert_equal TypeName.new(name: :Foo, namespace: Namespace.empty), m.name
        assert_equal [], m.args
        assert_equal "prepend Foo", m.location.source
      end
    end
  end

  def test_extension
    Parser.parse_signature(<<~SIG).yield_self do |decls|
      extension Array[X] (Pathname)
        @name: String

        include Foo
        extend Bar
        prepend Baz

        attr_reader path: X
        def Pathname: (String) -> Pathname
      end
    SIG
      decls[0].yield_self do |decl|
        assert_instance_of Declarations::Extension, decl
        assert_equal TypeName.new(name: :Array, namespace: Namespace.empty), decl.name
        assert_equal :Pathname, decl.extension_name
        assert_equal [:X], decl.type_params

        assert_equal 6, decl.members.size
      end
    end
  end
end
