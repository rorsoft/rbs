module RBS
  class Lexer
    class <<self
      def new_name
        @name_index ||= 0
        @name_index += 1
        "__lexer#{@name_index}__"
      end

      def token_defs
        @token_defs ||= []
      end

      def pat(string)
        case string
        when Regexp
          string
        when String
          Regexp.compile(Regexp.escape(string))
        end
      end

      def token(token, pattern, captures: 0)
        token_defs << [pat(pattern), captures, token, nil]
      end

      def token_invoke(method, pattern, captures: 0)
        token_defs << [pat(pattern), captures, nil, method]
      end

      def skip(pattern, captures: 0)
        token_defs << [pat(pattern), captures]
      end

      def regexp
        pats = token_defs.map do |tuple|
          pat = tuple[0]
          /(#{pat})/
        end

        Regexp.union(*pats)
      end
    end

    attr_reader :scanner

    def initialize(string)
      @index = 0
      @ret = []
      @scanner = CharScanner.new(string)

      @values = [nil]
      @start_poss = [nil]
      @end_poss = [nil]

      @regexp = self.class.regexp
      @defs = self.class.token_defs
    end

    def match?(pat)
      scanner.match?(pat)
    end

    def next_token
      return if scanner.eos?

      string = scanner.scan(@regexp)

      if string
        pat_index = 0
        @defs.each do |tuple|
          pat_index += 1 + tuple[1]
          if scanner[pat_index]
            if tuple.size == 2
              # skip
              return next_token
            else
              index = new_index()

              charpos = scanner.charpos
              @start_poss[index] = charpos - string.size
              @end_poss[index] = charpos

              if tok = tuple[2]
                @ret[0] = tok
                @values[index] = string
              else
                __send__(tuple[3], string) do |tok, value|
                  @ret[0] = tok
                  @values[index] = value
                end
              end

              @ret[1] = index

              return @ret
            end
          end
        end
      end

      on_error()
    end

    def semantic_value?(value)
      value.is_a?(Integer) && value < @values.size
    end

    def new_index
      @index += 1
      @index
    end

    def value(t)
      @values[t]
    end

    def start_pos(t)
      @start_poss[t]
    end

    def end_pos(t)
      @end_poss[t]
    end

    def range(t)
      start_pos(t)...end_pos(t)
    end

    def on_error()
      raise
    end
  end

  # class RBSLexer < Lexer
  #   skip(/\s+/)
  #   token_invoke(:quoted_ident, pattern: /`[a-zA-Z_]\w*`/)
  #   token_invoke(:quoted_method, pattern: /`(\\`|[^` :])+`/)
  #   token_invoke(:annotation, pattern: ANNOTATION_RE)
  #   token(:kSELFQ, pattern: /self\?/)
  #   token()
  # end
end
