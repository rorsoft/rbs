module RBS
  class Buffer
    attr_reader :name
    attr_reader :content
    attr_reader :lines
    attr_reader :ranges

    def initialize(name:, content:)
      @name = name
      @content = content

      @lines = content.lines

      @ranges = []
      offset = 0
      lines.each do |line|
        size = line.size
        range = offset...(offset+size)
        ranges << range
        offset += size
      end

      @locs = {}
    end

    def pos_to_loc(pos)
      @locs[pos] ||=
        begin
          index = ranges.bsearch_index do |range|
            pos < range.end ? true : false
          end

          if index
            [index + 1, pos - ranges[index].begin]
          else
            [ranges.size + 1, 0]
          end
        end
    end

    def loc_to_pos(loc)
      line, column = loc

      if range = ranges[line - 1]
        range.begin + column
      else
        last_position
      end
    end

    def last_position
      content.size
    end
  end
end
