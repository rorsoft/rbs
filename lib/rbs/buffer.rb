module RBS
  class Buffer
    attr_reader :name
    attr_reader :content
    attr_reader :lines
    attr_reader :ranges
    attr_reader :comments

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

      @locs = Array.new(content.size + 1)
      @comments = Array.new(lines.size)
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

    def insert_comment(string, start_pos, end_pos)
      start_line, start_column = pos_to_loc(start_pos)

      if (comment = leading_comment(line: start_line)) && comment.location.start_column == start_column
        comment.concat(string: "#{string}\n", range: start_pos...end_pos)
        @comments[start_line] = comment
      else
        location = Location.new(buffer: self, start_pos: start_pos, end_pos: end_pos)
        new_comment = AST::Comment.new(string: "#{string}\n", location: location)
        @comments[start_line] = new_comment
      end
    end

    def leading_comment(line:)
      comments[line - 1]
    end

    def last_position
      content.size
    end
  end
end
