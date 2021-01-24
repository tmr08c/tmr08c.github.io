In a [previous post](/2015/11/more-specific-factories), I wrote about creating more specific [FactoryBot](https://github.com/thoughtbot/factory_bot) factories using FactoryBot's [inheritance and nested factories](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inheritance) capabilities. 

I find I now rarely reach for leveraging inheritance with FactoryBot, and instead leverage [traits](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits). Similar to inheritance, you can use traits to create additional "presets" for your factories. One advantage of traits that has me reaching for them more often is the fact that you can combine multiple traits. Instead of relying on everything being defined as you need it in your nested factory, you can combine the pieces you need at test time to build the perfect factory.

In this post, I will cover some of the basics of traits. As you adopt them in your own project, I would recommend reading through the [full documentation](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits) as it covers more than I will here.

## Defining Our Models

For our example factories, we will be working with a `Camera` model. We will also have a `MemoryCard` model and a `Camera` can have zero or more `MemoryCard`s. While you may be more familiar with using FactoryBot in the context of Rails, you can create factories for any Ruby class. For our `Camera` and `MemoryCard`, we are going to use simple Ruby structs. Structs save us some of the work of defining an `initalize` method and will include more informnation when printed (without having to `#inspect` everytime).

As a result, our example models are simply:

```ruby
Camera = Struct.new(:manufacturer, :type, :memory_cards)
MemoryCard = Struct.new(:storage_capacity)
```

## A Basic Factory

For all models in my system, I am going to want a basic factory. This factory should include any required attributes I would need to create an instance of my model. While our models do not have any form of validation, we will pretend a `Camera` requires a `manufacturer` and a `type` and that a `MemoryCard` requires `storage_capacity`. With these requirements in mind, we can create our baseline factories:

```ruby
FactoryBot.define do
  factory :camera do
    manufacturer { 'Kodak' }
    type { :film }
  end

  factory :memory_card do
    storage_capacity { "32GB" }
  end
end
```

With this in place, we can now easily create instances of our models wthout the need to specify attributes every times:

```ruby
FactoryBot.build(:camera)
#<struct Camera manufacturer="Kodak", type=:film, memory_cards=nil>

FactoryBot.build(:memory_card)
#<struct MemoryCard storage_capacity="32GB">
```

## Adding Traits

Most of the time, you should be able to get away with only using your baseline factories. If you need to test some specific functionality that is impacted by a particular attribute, you can build an instance of your factory with that attribute directly in your test. For example, maybe we need to test  

```ruby
 FactoryBot.build(:camera, type: :mirrorless)
#<struct Camera manufacturer="Kodak", type=:mirrorless, memory_cards=nil>
```


TODO: Maybe change "type" to sensor size and have the example be calculating crop factor



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
require 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'factory_bot'
end

Camera = Struct.new(:manufacturer, :type, :memory_cards)
# class Camera
#   attr_writer :manufacturer, :type, :memory_cards

#   def initalize(manufacturer, type, memory_cards = [])
#     @manufacturer = manufacturer
#     @type = type
#     @memory_cards = memory_cards
#   end
# end

MemoryCard = Struct.new(:storage_capacity)
# class MemoryCard
#   attr_writer :storage_capacity

#   def initalize(storage_capacity)
#     @storage_capacity = storage_capacity
#   end
# end

FactoryBot.define do
  factory :camera do
    manufacturer { 'Kodak' }
    type { :film }

    trait :fujifilm do
      manufacturer { 'Fujifilm' }
    end

    trait :nikon do
      manufacturer { 'Nikon' }
    end

    trait :mirrorless do
      type { :mirrorless }
    end

    trait :dslr do
      type { :dslr }
    end

    factory :fujifilm_xt20 do
      fujifilm
      mirrorless
    end

    trait :with_memory_card do
      transient do
        number_of_cards { 1 }
      end

      memory_cards { number_of_cards.times.map { build(:memory_card) } }
    end
  end

  factory :memory_card do
    storage_capacity { "32GB" }
  end
end

puts FactoryBot.build(:camera).inspect

puts FactoryBot.build(:camera, manufacturer: "Sony")
puts FactoryBot.build(:camera, type: :mirrorless)


puts FactoryBot.build(:camera, :fujifilm)
puts FactoryBot.build(:camera, :nikon, type: :dslr)

puts FactoryBot.build(:camera, :nikon, :dslr)

puts FactoryBot.build(:fujifilm_xt20)


puts FactoryBot.build(:camera, :with_memory_card).inspect
puts FactoryBot.build(:camera, :nikon, :dslr, :with_memory_card, number_of_cards: 2).inspect
```

### Output

```ruby
› ruby factory_traits_test.rb
<struct Camera manufacturer="Kodak", type=:film, memory_cards=nil>
<struct Camera manufacturer="Sony", type=:film, memory_cards=nil>
<struct Camera manufacturer="Kodak", type=:mirrorless, memory_cards=nil>
<struct Camera manufacturer="Fujifilm", type=:film, memory_cards=nil>
<struct Camera manufacturer="Nikon", type=:dslr, memory_cards=nil>
<struct Camera manufacturer="Nikon", type=:dslr, memory_cards=nil>
<struct Camera manufacturer="Fujifilm", type=:mirrorless, memory_cards=nil>
<struct Camera manufacturer="Kodak", type=:film, memory_cards=[<struct MemoryCard storage_capacity="32GB">]>
<struct Camera manufacturer="Nikon", type=:dslr, memory_cards=[<struct MemoryCard storage_capacity="32GB">, <struct MemoryCard storage_capacity="32G
B">]>
```
