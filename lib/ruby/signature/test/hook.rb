require "ruby/signature"
require "pp"

module Ruby
  module Signature
    module Test
      class Hook
        class Error < Exception
          attr_reader :errors

          def initialize(errors)
            @errors = errors
            super "Type error detected: [#{errors.map {|e| Errors.to_string(e) }.join(", ")}]"
          end
        end

        attr_reader :env
        attr_reader :logger

        attr_reader :instance_module
        attr_reader :instance_methods
        attr_reader :singleton_module
        attr_reader :singleton_methods

        attr_reader :klass
        attr_reader :errors

        ArgsReturn = Struct.new(:arguments, :return_value, keyword_init: true)
        Call = Struct.new(:method_call, :block_call, :block_given, keyword_init: true)

        def builder
          @builder ||= DefinitionBuilder.new(env: env)
        end

        def typecheck
          @typecheck ||= TypeCheck.new(self_class: klass, builder: builder)
        end

        def initialize(env, klass, logger:, raise_on_error: false)
          @env = env
          @logger = logger
          @klass = klass

          @instance_module = Module.new
          @instance_methods = []

          @singleton_module = Module.new
          @singleton_methods = []

          @errors = []

          @raise_on_error = raise_on_error
        end

        def raise_on_error!(error = true)
          @raise_on_error = error
          self
        end

        def raise_on_error?
          @raise_on_error
        end

        def prepend!
          klass.prepend @instance_module
          klass.singleton_class.prepend @singleton_module

          if block_given?
            yield
            disable
          end

          self
        end

        def self.install(env, klass, logger:)
          new(env, klass, logger: logger).prepend!
        end

        def refinement
          klass = self.klass
          instance_module = self.instance_module
          singleton_module = self.singleton_module

          Module.new do
            refine klass do
              prepend instance_module
            end

            refine klass.singleton_class do
              prepend singleton_module
            end
          end
        end

        def verify_all
          type_name = Namespace.parse(klass.name).to_type_name.absolute!

          builder.build_instance(type_name).tap do |definition|
            definition.methods.each do |name, method|
              if method.defined_in.name.absolute! == type_name
                unless method.annotations.any? {|a| a.string == "rbs:test:skip" }
                  logger.info "Installing a hook on #{type_name}##{name}: #{method.method_types.join(" | ")}"
                  verify instance_method: name, types: method.method_types
                else
                  logger.info "Skipping test of #{type_name}##{name}"
                end
              end
            end
          end

          builder.build_singleton(type_name).tap do |definition|
            definition.methods.each do |name, method|
              if method.defined_in&.name&.absolute! == type_name || name == :new
                unless method.annotations.any? {|a| a.string == "rbs:test:skip" }
                  logger.info "Installing a hook on #{type_name}.#{name}: #{method.method_types.join(" | ")}"
                  verify singleton_method: name, types: method.method_types
                else
                  logger.info "Skipping test of #{type_name}.#{name}"
                end
              end
            end
          end

          self
        end

        def delegation(name, method_types, method_name)
          hook = self

          proc do |*args, &block|
            hook.logger.debug { "#{method_name} receives arguments: #{hook.inspect_(args)}" }

            block_calls = []

            if block
              original_block = block

              block = hook.call(Object.new, INSTANCE_EVAL) do |fresh_obj|
                proc do |*as|
                  hook.logger.debug { "#{method_name} receives block arguments: #{hook.inspect_(as)}" }

                  ret = if self.equal?(fresh_obj)
                          original_block[*as]
                        else
                          hook.call(self, INSTANCE_EXEC, *as, &original_block)
                        end

                  block_calls << ArgsReturn.new(arguments: as, return_value: ret)

                  hook.logger.debug { "#{method_name} returns from block: #{hook.inspect_(ret)}" }

                  ret
                end
              end
            end

            method = hook.call(self, METHOD, name)
            klass = hook.call(self, CLASS)
            singleton_klass = begin
              hook.call(self, SINGLETON_CLASS)
            rescue TypeError
              nil
            end
            prepended = klass.ancestors.include?(hook.instance_module) || singleton_klass&.ancestors&.include?(hook.singleton_module)
            result = if prepended
                       method.super_method.call(*args, &block)
                     else
                       # Using refinement
                       method.call(*args, &block)
                     end

            hook.logger.debug { "#{method_name} returns: #{hook.inspect_(result)}" }

            calls = if block_calls.empty?
                      [Call.new(method_call: ArgsReturn.new(arguments: args, return_value: result),
                                block_call: nil,
                                block_given: block != nil)]
                    else
                      block_calls.map do |block_call|
                        Call.new(method_call: ArgsReturn.new(arguments: args, return_value: result),
                                 block_call: block_call,
                                 block_given: block != nil)
                      end
                    end

            errorss = []

            method_types.each do |method_type|
              yield_errors = calls.map do |call|
                hook.test(method_name, method_type, call)
              end.reject(&:empty?)

              if yield_errors.empty?
                errorss << []
              else
                errorss.push(*yield_errors)
              end
            end

            new_errors = []

            if errorss.none?(&:empty?)
              if (best_errors = hook.find_best_errors(errorss))
                new_errors.push(*best_errors)
              else
                new_errors << TypeCheck::Errors::UnresolvedOverloadingError.new(
                  klass: hook.klass,
                  method_name: method_name,
                  method_types: method_types
                )
              end
            end

            unless new_errors.empty?
              new_errors.each do |error|
                hook.logger.error Errors.to_string(error)
              end

              hook.errors.push(*new_errors)

              if hook.raise_on_error?
                raise Error.new(new_errors)
              end
            end

            result
          end.ruby2_keywords
        end

        def verify(instance_method: nil, singleton_method: nil, types:)
          method_types = types.map do |type|
            case type
            when String
              Parser.parse_method_type(type)
            else
              type
            end
          end

          case
          when instance_method
            instance_methods << instance_method
            call(self.instance_module, DEFINE_METHOD, instance_method, &delegation(instance_method, method_types, "##{instance_method}"))
          when singleton_method
            call(self.singleton_module, DEFINE_METHOD, singleton_method, &delegation(singleton_method, method_types, ".#{singleton_method}"))
          end

          self
        end

        def find_best_errors(errorss)
          if errorss.size == 1
            errorss[0]
          else
            no_arity_errors = errorss.select do |errors|
              errors.none? do |error|
                error.is_a?(TypeCheck::Errors::ArgumentError) ||
                  error.is_a?(TypeCheck::Errors::BlockArgumentError) ||
                  error.is_a?(TypeCheck::Errors::MissingBlockError) ||
                  error.is_a?(TypeCheck::Errors::UnexpectedBlockError)
              end
            end

            unless no_arity_errors.empty?
              # Choose a error set which doesn't include arity error
              return no_arity_errors[0] if no_arity_errors.size == 1
            end
          end
        end

        def self.backtrace(skip: 2)
          raise
        rescue => exn
          exn.backtrace.drop(skip)
        end

        def test(method_name, method_type, call)
          errors = []

          typecheck_args(method_name, method_type, method_type.type, call.method_call, errors, type_error: Errors::ArgumentTypeError, argument_error: Errors::ArgumentError)
          typecheck_return(method_name, method_type, method_type.type, call.method_call, errors, return_error: Errors::ReturnTypeError)

          if method_type.block
            case
            when call.block_call
              # Block is yielded
              typecheck_args(method_name, method_type, method_type.block.type, call.block_call, errors, type_error: Errors::BlockArgumentTypeError, argument_error: Errors::BlockArgumentError)
              typecheck_return(method_name, method_type, method_type.block.type, call.block_call, errors, return_error: Errors::BlockReturnTypeError)
            when !call.block_given
              # Block is not given
              if method_type.block.required
                errors << Errors::MissingBlockError.new(klass: klass, method_name: method_name, method_type: method_type)
              end
            else
              # Block is given, but not yielded
            end
          else
            if call.block_given
              errors << Errors::UnexpectedBlockError.new(klass: klass, method_name: method_name, method_type: method_type)
            end
          end

          errors
        end

        def run
          yield
          self
        ensure
          disable
        end

        def call(receiver, method, *args, &block)
          method.bind(receiver).call(*args, &block)
        end

        def inspect_(obj)
          Hook.inspect_(obj)
        end

        def self.inspect_(obj)
          obj.inspect
        rescue
          INSPECT.bind(obj).call()
        end

        def disable
          self.instance_module.remove_method(*instance_methods)
          self.singleton_module.remove_method(*singleton_methods)
          self
        end

        def typecheck_args(method_name, method_type, fun, value, errors, type_error:, argument_error:)
          test = zip_args(value.arguments, fun) do |value, param|
            unless typecheck.check(value, param.type)
              errors << type_error.new(klass: klass,
                                       method_name: method_name,
                                       method_type: method_type,
                                       param: param,
                                       value: value)
            end
          end

          unless test
            errors << argument_error.new(klass: klass,
                                         method_name: method_name,
                                         method_type: method_type)
          end
        end

        def typecheck_return(method_name, method_type, fun, value, errors, return_error:)
          unless typecheck.check(value.return_value, fun.return_type)
            errors << return_error.new(klass: klass,
                                       method_name: method_name,
                                       method_type: method_type,
                                       type: fun.return_type,
                                       value: value.return_value)
          end
        end

        def keyword?(value)
          value.is_a?(Hash) && value.keys.all? {|key| key.is_a?(Symbol) }
        end

        def zip_keyword_args(hash, fun)
          fun.required_keywords.each do |name, param|
            if hash.key?(name)
              yield(hash[name], param)
            else
              return false
            end
          end

          fun.optional_keywords.each do |name, param|
            if hash.key?(name)
              yield(hash[name], param)
            end
          end

          hash.each do |name, value|
            next if fun.required_keywords.key?(name)
            next if fun.optional_keywords.key?(name)

            if fun.rest_keywords
              yield value, fun.rest_keywords
            else
              return false
            end
          end

          true
        end

        def zip_args(args, fun, &block)
          case
          when args.empty?
            if fun.required_positionals.empty? && fun.trailing_positionals.empty? && fun.required_keywords.empty?
              true
            else
              false
            end
          when !fun.required_positionals.empty?
            yield_self do
              param, fun_ = fun.drop_head
              yield(args.first, param)
              zip_args(args.drop(1), fun_, &block)
            end
          when fun.has_keyword?
            yield_self do
              hash = args.last
              if keyword?(hash)
                zip_keyword_args(hash, fun, &block) &&
                  zip_args(args.take(args.size - 1),
                           fun.update(required_keywords: {}, optional_keywords: {}, rest_keywords: nil),
                           &block)
              else
                fun.required_keywords.empty? &&
                  zip_args(args,
                           fun.update(required_keywords: {}, optional_keywords: {}, rest_keywords: nil),
                           &block)
              end
            end
          when !fun.trailing_positionals.empty?
            yield_self do
              param, fun_ = fun.drop_tail
              yield(args.last, param)
              zip_args(args.take(args.size - 1), fun_, &block)
            end
          when !fun.optional_positionals.empty?
            yield_self do
              param, fun_ = fun.drop_head
              yield(args.first, param)
              zip_args(args.drop(1), fun_, &block)
            end
          when fun.rest_positionals
            yield_self do
              yield(args.first, fun.rest_positionals)
              zip_args(args.drop(1), fun, &block)
            end
          else
            false
          end
        end
      end
    end
  end
end
