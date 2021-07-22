module RBS
  module AST
    class Comment
      attr_reader :location

      def initialize(string:, location:)
        @string = [string]
        @location = location
      end

      def string
        @string.join
      end

      def ==(other)
        other.is_a?(Comment) && other.string == string
      end

      alias eql? ==

      def hash
        self.class.hash ^ @string.hash
      end

      def to_json(state = _ = nil)
        { string: string, location: location }.to_json(state)
      end

      def concat(string:, location:)
        @string << string

        if loc = @location
          loc.concat location
        else
          @location = location
        end

        self
      end
    end
  end
end
