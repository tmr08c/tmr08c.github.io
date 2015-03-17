require 'rspec'

describe NilClass do
  subject { nil }

  it 'should be false' do
    expect(subject).to be_falsy
  end
end

Finished in 0.00151 seconds (files took 0.11127 seconds to load)
1 example, 0 failures
