


# Brainstorm

* Update of `/2015/11/more-specific-factories`
    * Rename FactoryGirl to FactoryBot
    *

* Last time used inheritance
    * https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inheritance

```ruby
# factories.rb

factory :person do
  name 'Jane Doe'
  sex 'female'
  age 18

  factory :female_person do
    sex 'female'
  end

  factory :male_peson do
    sex 'male'
  end
end
```

* This time, traits https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits

* Can use inheritance and traits together https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#as-implicit-attributes-1

* Maybe include example using transient attributes as well (https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#with-transient-attributes) since we've used this at work

## Example

```ruby
class Camera
  def initialize(manufacturer:, type: (dslr, mirrorless))
  end
end
```

Facotries

```ruby
factory :camera do
  manufacturer { 'my camera manufacturer' }
  type { 'picture taking kind' }

  trait: :canon do
    manufacturer { 'canon' }
  end

  trait: :fujifilm do
    manufacturer: :fujifilm
  end

  trait: :dslr do
    type: :dslr
  end

  trait: :mirrorless do
    type: :mirrorless
  end
end
```

```ruby
factory :fujifilm_xt20, parent: :camera do
  fujifilm
  mirrorless
end
