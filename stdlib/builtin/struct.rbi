# A [Struct](Struct) is a convenient way to bundle a
# number of attributes together, using accessor methods, without having to
# write an explicit class.
# 
# The [Struct](Struct) class generates new subclasses
# that hold a set of members and their values. For each member a reader
# and writer method is created similar to
# [Module\#attr\_accessor](https://ruby-doc.org/core-2.6.3/Module.html#method-i-attr_accessor)
# .
# 
# ```ruby
# Customer = Struct.new(:name, :address) do
#   def greeting
#     "Hello #{name}!"
#   end
# end
# 
# dave = Customer.new("Dave", "123 Main")
# dave.name     #=> "Dave"
# dave.greeting #=> "Hello Dave!"
# ```
# 
# See [::new](Struct#method-c-new) for further
# examples of creating struct subclasses and instances.
# 
# In the method descriptions that follow, a "member" parameter refers to a
# struct member which is either a quoted string ( `"name"` ) or a
# [Symbol](https://ruby-doc.org/core-2.6.3/Symbol.html) ( `:name` ).
class Struct[Elem] < Object
  include Enumerable

  def initialize: (Symbol | String arg0, *Symbol | String arg1, ?keyword_init: bool keyword_init) -> void

  def each: () { (Elem arg0) -> any } -> any
          | () -> self

  def self.members: () -> ::Array[Symbol]

  def new: (*any args) -> Struct
end
