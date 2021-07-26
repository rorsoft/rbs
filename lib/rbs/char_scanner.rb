module RBS
  class CharScanner < StringScanner
    def initialize(string)
      super(string)
      @charpos = 0
    end

    alias original_charpos charpos

    def charpos
      @charpos
    end

    def scan(pattern)
      s = super
      @charpos += s.size if s
      s
    end

    def skip(pattern)
      size = super
      if size
        @charpos += slice_chars(pos - size, size)
      end
    end

    def skip_chars(pattern)
      charpos = @charpos
      skip(pattern)
      @charpos - charpos
    end

    # def skip(pattern)
    #   s = super
    #   @charpos += s if s
    #   s
    # end
  end
end
