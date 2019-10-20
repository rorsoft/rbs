require "optparse"

module Ruby
  module Signature
    class CLI
      class LibraryOptions
        attr_reader :libs
        attr_reader :dirs
        attr_accessor :no_stdlib

        def initialize()
          @libs = []
          @dirs = []
          @no_stdlib = false
        end

        def setup(loader)
          libs.each do |lib|
            loader.add(library: lib)
          end

          dirs.each do |dir|
            loader.add(path: Pathname(dir))
          end

          loader.stdlib_root = nil if no_stdlib

          loader
        end
      end

      attr_reader :stdout
      attr_reader :stderr

      def initialize(stdout:, stderr:)
        @stdout = stdout
        @stderr = stderr
      end

      COMMANDS = [:ast, :list, :ancestors, :methods, :method, :validate, :constant, :paths, :scaffold, :version]

      def library_parse(opts, options:)
        opts.on("-r LIBRARY") do |lib|
          options.libs << lib
        end

        opts.on("-I DIR") do |dir|
          options.dirs << dir
        end

        opts.on("--no-stdlib") do
          options.no_stdlib = true
        end

        opts
      end

      def run(args)
        options = LibraryOptions.new

        OptionParser.new do |opts|
          library_parse(opts, options: options)
        end.order!(args)

        command = args.shift&.to_sym

        if COMMANDS.include?(command)
          __send__ :"run_#{command}", args, options
        else
          run_help()
        end
      end

      def run_help
        stdout.puts "Available commands: #{COMMANDS.join(", ")}"
      end

      def run_ast(args, options)
        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        stdout.print JSON.generate(env.declarations)
        stdout.flush
      end

      def run_list(args, options)
        list = []

        OptionParser.new do |opts|
          opts.on("--class") { list << :class }
          opts.on("--module") { list << :module }
          opts.on("--interface") { list << :interface }
        end.order!(args)

        list.push(:class, :module, :interface) if list.empty?

        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        env.each_decl.sort_by {|name,| name.to_s }.each do |type_name, decl|
          case decl
          when AST::Declarations::Class
            if list.include?(:class)
              stdout.puts "#{type_name} (class)"
            end
          when AST::Declarations::Module
            if list.include?(:module)
              stdout.puts "#{type_name} (module)"
            end
          when AST::Declarations::Interface
            if list.include?(:interface)
              stdout.puts "#{type_name} (interface)"
            end
          end
        end
      end

      def run_ancestors(args, options)
        kind = :instance

        OptionParser.new do |opts|
          opts.on("--instance") { kind = :instance }
          opts.on("--singleton") { kind = :singleton }
        end.order!(args)

        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        builder = DefinitionBuilder.new(env: env)
        type_name = parse_type_name(args[0]).absolute!

        if env.class?(type_name)
          ancestor = case kind
                     when :instance
                       decl = env.find_class(type_name)
                       Definition::Ancestor::Instance.new(name: type_name,
                                                          args: Types::Variable.build(decl.type_params.each.map(&:name)))
                     when :singleton
                       Definition::Ancestor::Singleton.new(name: type_name)
                     end

          ancestors = builder.build_ancestors(ancestor)

          ancestors.each do |ancestor|
            case ancestor
            when Definition::Ancestor::Singleton
              stdout.puts "singleton(#{ancestor.name})"
            when Definition::Ancestor::ExtensionSingleton
              stdout.puts "singleton(#{ancestor.name} (#{ancestor.extension_name}))"
            when Definition::Ancestor::Instance
              if ancestor.args.empty?
                stdout.puts ancestor.name.to_s
              else
                stdout.puts "#{ancestor.name}[#{ancestor.args.join(", ")}]"
              end
            when Definition::Ancestor::ExtensionInstance
              if ancestor.args.empty?
                stdout.puts "#{ancestor.name} (#{ancestor.extension_name})"
              else
                stdout.puts "#{ancestor.name}[#{ancestor.args.join(", ")}] (#{ancestor.extension_name})"
              end
            end
          end
        else
          stdout.puts "Cannot find class: #{type_name}"
        end
      end

      def run_methods(args, options)
        kind = :instance
        inherit = true

        OptionParser.new do |opts|
          opts.on("--instance") { kind = :instance }
          opts.on("--singleton") { kind = :singleton }
          opts.on("--inherit") { inherit = true }
          opts.on("--no-inherit") { inherit = false }
        end.order!(args)

        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        builder = DefinitionBuilder.new(env: env)
        type_name = parse_type_name(args[0]).absolute!

        if env.class?(type_name)
          definition = case kind
                       when :instance
                         builder.build_instance(type_name)
                       when :singleton
                         builder.build_singleton(type_name)
                       end

          definition.methods.keys.sort.each do |name|
            method = definition.methods[name]
            if inherit || method.implemented_in == definition.declaration
              stdout.puts "#{name} (#{method.accessibility})"
            end
          end
        else
          stdout.puts "Cannot find class: #{type_name}"
        end
      end

      def run_method(args, options)
        kind = :instance

        OptionParser.new do |opts|
          opts.on("--instance") { kind = :instance }
          opts.on("--singleton") { kind = :singleton }
        end.order!(args)

        unless args.size == 2
          stdout.puts "Expected two arguments, but given #{args.size}."
          return
        end

        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        builder = DefinitionBuilder.new(env: env)
        type_name = parse_type_name(args[0]).absolute!
        method_name = args[1].to_sym

        unless env.class?(type_name)
          stdout.puts "Cannot find class: #{type_name}"
          return
        end

        definition = case kind
                     when :instance
                       builder.build_instance(type_name)
                     when :singleton
                       builder.build_singleton(type_name)
                     end

        method = definition.methods[method_name]

        unless method
          stdout.puts "Cannot find method: #{method_name}"
          return
        end

        stdout.puts "#{type_name}#{kind == :instance ? "#" : "."}#{method_name}"
        stdout.puts "  defined_in: #{method.defined_in&.name&.absolute!}"
        stdout.puts "  implementation: #{method.implemented_in.name.absolute!}"
        stdout.puts "  accessibility: #{method.accessibility}"
        stdout.puts "  types:"
        separator = " "
        for type in method.method_types
          stdout.puts "    #{separator} #{type}"
          separator = "|"
        end
      end

      def run_validate(args, options)
        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        builder = DefinitionBuilder.new(env: env)

        env.each_decl do |name, decl|
          case decl
          when AST::Declarations::Class, AST::Declarations::Module
            stdout.puts "#{Location.to_string decl.location}:\tValidating class/module definition: `#{name}`..."
            builder.build_instance(decl.name.absolute!).each_type do |type|
              env.validate type, namespace: Namespace.root
            end
            builder.build_singleton(decl.name.absolute!).each_type do |type|
              env.validate type, namespace: Namespace.root
            end
          when AST::Declarations::Interface
            stdout.puts "#{Location.to_string decl.location}:\tValidating interface: `#{name}`..."
            builder.build_interface(decl.name.absolute!, decl).each_type do |type|
              env.validate type, namespace: Namespace.root
            end
          end
        end

        env.each_constant do |name, const|
          stdout.puts "#{Location.to_string const.location}:\tValidating constant: `#{name}`..."
          env.validate const.type, namespace: name.namespace
        end

        env.each_global do |name, global|
          stdout.puts "#{Location.to_string global.location}:\tValidating global: `#{name}`..."
          env.validate global.type, namespace: Namespace.root
        end

        env.each_alias do |name, decl|
          stdout.puts "#{Location.to_string decl.location}:\tValidating alias: `#{name}`..."
          env.validate decl.type, namespace: name.namespace
        end
      end

      def run_constant(args, options)
        context = nil

        OptionParser.new do |opts|
          opts.on("--context CONTEXT") {|c| context = c }
        end.order!(args)

        unless args.size == 1
          stdout.puts "Expected one argument."
          return
        end

        loader = EnvironmentLoader.new()

        options.setup(loader)

        env = Environment.new()
        loader.load(env: env)

        builder = DefinitionBuilder.new(env: env)
        table = ConstantTable.new(builder: builder)

        namespace = context ? Namespace.parse(context).absolute! : Namespace.root
        stdout.puts "Context: #{namespace}"
        name = Namespace.parse(args[0]).to_type_name
        stdout.puts "Constant name: #{name}"

        constant = table.resolve_constant_reference(name, context: namespace)

        if constant
          stdout.puts " => #{constant.name}: #{constant.type}"
        else
          stdout.puts " => [no constant]"
        end
      end

      def run_version(args, options)
        stdout.puts "ruby-signature #{VERSION}"
      end

      def run_paths(args, options)
        loader = EnvironmentLoader.new()

        options.setup(loader)

        kind_of = -> (path) {
          case
          when path.file?
            "file"
          when path.directory?
            "dir"
          when !path.exist?
            "absent"
          else
            "unknown"
          end
        }

        if loader.stdlib_root
          path = loader.stdlib_root
          stdout.puts "#{path}/builtin (#{kind_of[path]}, stdlib)"
        end

        loader.paths.each do |path|
          case path
          when Pathname
            stdout.puts "#{path} (#{kind_of[path]})"
          when EnvironmentLoader::GemPath
            stdout.puts "#{path.path} (#{kind_of[path.path]}, gem, name=#{path.name}, version=#{path.version})"
          when EnvironmentLoader::LibraryPath
            stdout.puts "#{path.path} (#{kind_of[path.path]}, library, name=#{path.name})"
          end
        end
      end

      def run_scaffold(args, options)
        format = args.shift

        parser = case format
                 when "rbi"
                   Scaffold::RBI.new()
                 end

        args.each do |file|
          parser.parse Pathname(file).read
        end

        writer = Writer.new(out: stdout)
        writer.write parser.decls
      end

      def parse_type_name(string)
        Namespace.parse(string).yield_self do |namespace|
          last = namespace.path.last
          TypeName.new(name: last, namespace: namespace.parent)
        end
      end
    end
  end
end
