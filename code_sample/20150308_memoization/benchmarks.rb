require 'benchmark'

class NoMemo
  def call
    evens + odds
  end

  def evens
    long_running_value_getter.select { |v| v % 2 == 0 }
  end

  def odds
    long_running_value_getter.select { |v| v % 2 != 0 }
  end

  private

  def long_running_value_getter
    sleep 5
    (1..10_000).to_a
  end
end

class MoMemo
  def call
    evens + odds
  end

  def evens
    values.select { |v| v % 2 == 0 }
  end

  def odds
    values.select { |v| v % 2 != 0 }
  end

  private

  def values
    @values ||= long_running_value_getter
  end

  def long_running_value_getter
    sleep 5
    (1..10_000).to_a
  end
end


Benchmark.bmbm do |x|
  x.report('No memoization') { NoMemo.new.call }
  x.report('Memoization') { MoMemo.new.call }
end

