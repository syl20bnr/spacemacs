class HelloWorld
  # Top level class
  attr_reader :hello_s

  def initialize(hello_s:)
    @hello_s = hello_s
  end

  def say_hello
    puts hello_s
  end

  def run
    say_hello
  end
end

hello_world = HelloWorld.new(hello_s: 'Hello')
hello_world.say_hello
