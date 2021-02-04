---
title: "Creating More Specific Factories with Traits"
date: '2021-03-20T06:14:13.265Z'
categories: ['testing', 'ruby']
---

In a [previous post](/2015/11/more-specific-factories), I wrote about creating more specific [FactoryBot](https://github.com/thoughtbot/factory_bot) factories using FactoryBot's [inheritance and nested factories](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inheritance) capabilities. In this post, I will cover my current practices for creating more specific factories which rely on [traits](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits) instead.

Similar to inheritance, you can use traits to create additional "presets" for your factories. One advantage of traits that has me reaching for them more often is the fact that you can combine multiple traits. Instead of relying on everything being defined as you need it in your nested factory, you can combine the pieces you need at test time to build the perfect factory.

In this post, I will cover the basics of using traits. As you adopt them in your own project, I would recommend reading through the [full documentation](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits) as it covers more than I will here.

## Defining Our Models

For our example factories, we will be working with two models: `Camera` and `MemoryCard`, and a `Camera` can have zero [or more](https://www.howtogeek.com/392378/what’s-the-big-deal-about-dual-storage-card-slots-for-cameras/) `MemoryCard`s.

While you may be more familiar with using FactoryBot in the context of Rails, you can create factories for any Ruby class. For our `Camera` and `MemoryCard`, we are going to use simple Ruby [`Struct`](https://ruby-doc.org/core/Struct.html)s. As a result, our example models are simply:

```ruby
Camera = Struct.new(:manufacturer, :frame_size, :memory_cards)
MemoryCard = Struct.new(:storage_capacity)
```

## A Basic Factory

For all models in my system, I am going to want a basic factory. This factory should include any required attributes I would need to create an instance of my model. While our models do not have any form of validation, we will pretend a `Camera` requires a `manufacturer` and a `frame_size`, and that a `MemoryCard` requires `storage_capacity`. With these requirements in mind, we can create our baseline factories:

```ruby
FactoryBot.define do
  factory :camera do
    manufacturer { 'Kodak' }
    frame_size { '35x24' }
  end

  factory :memory_card do
    storage_capacity { "32GB" }
  end
end
```

With this in place, we can now easily create instances of our models without the need to specify attributes every times:

```ruby
FactoryBot.build(:camera)
=> <struct Camera manufacturer="Kodak", frame_size="35x24", memory_cards=nil>

FactoryBot.build(:memory_card)
=> <struct MemoryCard storage_capacity="32GB">
```

## Adding Traits

Much of the time, you can get away with using your baseline factories. If you need to test some specific functionality that is impacted by a particular attribute, you can build an instance of your factory with that attribute directly in your test. For example, maybe we need to test how we calculate crop factor. Since the crop factor is a [calculation that is based on the frame size](https://shuttermuse.com/calculate-cameras-crop-factor/), we could create factories with various frame sizes:


```ruby
describe '#crop_factor' do
  context 'when working with a full frame camera' do
    let(:camera) { FactoryBot.build(:camera, frame_size: "35x24") }
  end

  context 'when working with a APS-C camera' do
    let(:camera) { FactoryBot.build(:camera, frame_size: "6.17x4.55") }
  end
end
```

However, we may find we need to set `frame_size` to a few common dimensions in multiple tests or that manually entering in the `framze_size` can be error prone. This could be a case where we consider defining some traits.

The syntax for creating a trait is similar to creating a factory. We have a `trait` block that we use to name our trait and within the trait we use the same syntax to set values for our attributes. Below, we've defined traits that represent common sensor types, specifically, their frame sizes.

```ruby
factory :camera do
  trait :full_frame do
    frame_size { '35x24' }
  end

  trait :aps_c do
    frame_size { '23.6x15.6' }
  end
end
```

In our test, we can now create instances from our factory by referencing the trait and not having to know exact dimensions for common frames.

```ruby
FactoryBot.build(:camera, :aps_c)
=> <struct Camera manufacturer="Kodak", frame_size="23.6x15.6", memory_cards=nil>

# we can still override other attributes if we want
FactoryBot.build(:camera, :full_frame, manufacturer: "Nikon")
=> <struct Camera manufacturer="Nikon", frame_size="35x24", memory_cards=nil>
```

As you can see above, traits will still "inherit" the default values set up in our baseline factory and we can also still override any individual attributes.

Now that we've seen the trait, I want to mention that, for this example, it _could_ make sense to directly set `frame_size` when creating our factories. Since `crop_factor` is a mathematical formula based on `frame_size`, it may be easier to understand our test expectations if we see the actual `frame_size` as opposed to having it abstracted away. This reveals some of the subtly of dealing with traits and the potential to introduce [mystery guests](https://thoughtbot.com/blog/mystery-guest). For a casual photographer like myself, the relationship between `frame_size` and `crop_factor` is fuzzy, but having the number explicitly set in the test could provide a signal of how the calculation is done. On the other hand, you will likely find an experienced team working on this application will have industry knowledge and know `full_frame` means  "35x24" and how that impacts the `crop_factor` calculation. This is a balance you may want to experiment with as you introduce traits into your tests.

## Combining Traits

One of the primary reasons I find myself reaching for traits over inheritance is the ability to combine multiple traits when creating new factories.

Let's imagine we needed the ability to search for camera by various attributes. Starting with a single attribute, we can use our existing trait for `frame_size` and write tests like the following.

```ruby
describe '#search' do
  context 'when searching by frame size' do
    let(:full_frame_camera) { build(:camera, :full_frame) }
    let(:aps_c_camera) { build(:camera, :aps_c) }
  end
end
```

Our current context only deals with searching for a single attribute, but we want our search ability to handle filtering on multiple attributes. To test this, we can try searching on `frame_size` **and** `manufacturer`. Before we write the test, let's create a few traits for manufacturers.

```ruby
factory :camera do
  trait :fujifilm do
    manufacturer { 'Fujifilm' }
  end

  trait :nikon do
    manufacturer { 'Nikon' }
  end
end
```

We can now combine our different traits just like we would specify attributes when creating a factory.

```ruby
FactoryBot.build(:camera, :aps_c, :fujifilm)
=> <struct Camera manufacturer="Fujifilm", frame_size="23.6x15.6", memory_cards=nil>

FactoryBot.build(:camera, :nikon, :full_frame)
=> <struct Camera manufacturer="Nikon", frame_size="35x24", memory_cards=nil>
```

While this is equivalent to specifying the `frame_size` and `manufacturer` attributes directly, with well-named traits you can quickly see what is being set without the need to specify every attribute. The flexibility provided by traits also makes it easy for us to build up more varied combinations in our tests.

```ruby
describe '#search' do
  context 'when searching by multiple attributes' do
    let(:full_frame_nikon) { build(:camera, :full_frame, :nikon) }
    let(:aps_c_nikon) { build(:camera, :aps_c, :nikon) }
    let(:aps_c_fujifilm) { build(:camera, :aps_c, :fujifilm) }
  end
end
```

## Working with Associations

Without traits, if we wanted our camera to include a memory card, we would use a factory to create a memory card and pass that in when building out camera.

```ruby
context 'when a camera has a memory card' do
  let(:memory_card) { build(:memory_card) }
  let(:camera) { build(:camera, memory_cards: [memory_card]) }
end
```

This provides us with the flexibility to customize the `memory_cards` we supply our camera with. However, we may find we often just want to make sure a camera has a memory card and not care about the `storage_capacity`. In that case, explicitly creating an instance of a memory card in our test could add noise. This is another place where we can leverage a trait.

We can create a trait on our `Camera` model that indicates this camera includes a `MemoryCard`.

```ruby
factory :camera do
  trait :with_memory_card do
    # Since `memory_cards` expects an array,
    # we create a single element array
    memory_cards { [build(:memory_card)] }
  end
end
```

Our trait sets our `memory_cards` association to be a single-element array with a factory-built `MemoryCard`. We are able to simply `build` our association because we are working with `Struct`s and not `ActiveRecord`-backed models. If you are in a Rails app, you will want to review the different options for [specifying an association](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#associations) with FactoryBot and decide which makes sense for your situation.

```ruby
FactoryBot.build(:camera, :with_memory_card)
=> <struct Camera
          manufacturer="Kodak",
          frame_size="35x24",
          memory_cards=[
            <struct MemoryCard storage_capacity="32GB">
          ]>
```

We can now update our example above to now longer explicitly create the memory card.

```ruby
context 'when a camera has a memory card' do
  let(:camera) { build(:camera, :with_memory_card) }
end
```

### Leveraging Transient Attributes

We can go a step further with out trait and add the ability to specify how many memory cards a `Camera` has. For this, we can use [transient attributes](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#transient-attributes). Transient attributes allow you to pass variables into FactoryBot that are not a part of the model you are creating, but can be referenced during during the creation of the factory. Let's take a look at this in practice:

```ruby
factory :camera do
  trait :with_memory_card do
    transient do
      number_of_cards { 1 }
    end

    memory_cards { Array.new(number_of_cards) { build(:memory_card) } }
  end
end
```

The syntax for transient attributes is very similar to the syntax of setting attributes on a factory. Here, we have a `transient` block that sets up an attribute, `number_of_cards` and defaults it to `1`. Now, when we create our `memory_cards` associations, we create an `Array` with `number_of_cards` elements where each element defaults to a new, factory-built instance of a `MemoryCard`.

Just like we can explicitly set attributes when creating a factory, we can explicitly set our transient attributes as well.

```ruby
# use default value for number_of_cards
FactoryBot.build(:camera, :nikon, :full_frame, :with_memory_card)
=> <struct Camera
          manufacturer="Nikon",
          frame_size="35x24",
          memory_cards=[
            <struct MemoryCard storage_capacity="32GB">
          ]>

# set value for number_of_cards
FactoryBot.build(:camera, :nikon, :full_frame, :with_memory_card, number_of_cards: 2)
=> <struct Camera
          manufacturer="Nikon",
          frame_size="35x24",
          memory_cards=[
            <struct MemoryCard storage_capacity="32GB">,
            <struct MemoryCard storage_capacity="32GB">
          ]>
```

This pattern is great for tests that interact with associated records and don't require special set up for the associated objects.

```ruby
describe '#store_picture' do
  context 'when there are multiple memory cards' do
    let(:camera) { FactoryBot.build(:camera, :with_memory_card, number_of_cards: 2) }
  end
end
```

## Inheritance is still on the table

While I find I don't often reach for the option of inheritance anymore, it is still something that may make sense and using traits does not prevent you from also using inheritance. In fact, you can build nested factories that leverage your traits.

```ruby
factory :camera do
  factory :wedding_camera do
    full_frame
    number_of_cards { 2 }
    with_memory_card
  end


  factory :prosumer_camera do
    manufacturer { 'Sony' }
    aps_c
    with_memory_card
  end
end

FactoryBot.build(:wedding_camera)
=> <struct Camera manufacturer="Kodak", frame_size="35x24", memory_cards=[<struct MemoryCard storage_capacity="32GB">, <struct MemoryCard storage_capacity="32GB">]>

FactoryBot.build(:prosumer_camera)
=>  <struct Camera manufacturer="Sony", frame_size="23.6x15.6", memory_cards=[<struct MemoryCard storage_capacity="32GB">]>
```

This example shows how we can use traits to build up our more specific, inherited factories.

# Conclusion

Traits provide reusable blocks for building factories. They often result in tests that are easier to read and provide a way to name concepts that are important to a model in your system. While you don't want to set every attribute via trait, I find traits to be a go-to feature of FactoryBot for me.


## Example

```ruby
rrequire 'bundler/inline'

gemfile do
  source 'https://rubygems.org'

  gem 'factory_bot'
end

Camera = Struct.new(:manufacturer, :frame_size, :memory_cards)
MemoryCard = Struct.new(:storage_capacity)

factorybot.define do
  factory :camera do
    manufacturer { 'kodak' }
    frame_size { '35x24' }

    trait :fujifilm do
      manufacturer { 'fujifilm' }
    end

    trait :nikon do
      manufacturer { 'nikon' }
    end

    trait :full_frame do
      frame_size { '35x24' }
    end

    trait :asp_c do
      frame_size { '23.6x15.6' }
    end

    trait :micro_4_3rd do
      frame_size { '17x13' }
    end

    factory :fujifilm_xt20 do
      fujifilm
      asp_c
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
puts FactoryBot.build(:memory_card).inspect

puts FactoryBot.build(:camera, frame_size: :asp_c)

puts FactoryBot.build(:camera, manufacturer: "Sony")
puts FactoryBot.build(:camera, frame_size: :micro_4_3rd)


puts FactoryBot.build(:camera, :fujifilm)
puts FactoryBot.build(:camera, :nikon, frame_size: :full_frame)

puts FactoryBot.build(:camera, :nikon, :asp_c)

puts FactoryBot.build(:fujifilm_xt20)


puts FactoryBot.build(:camera, :with_memory_card).inspect
puts FactoryBot.build(:camera, :nikon, :full_frame, :with_memory_card, number_of_cards: 2).inspect
```

Result

```ruby
<struct Camera manufacturer="Kodak", frame_size="35x24", memory_cards=nil>
<struct MemoryCard storage_capacity="32GB">
<struct Camera manufacturer="Kodak", frame_size=:asp_c, memory_cards=nil>
<struct Camera manufacturer="Sony", frame_size="35x24", memory_cards=nil>
<struct Camera manufacturer="Kodak", frame_size=:micro_4_3rd, memory_cards=nil>
<struct Camera manufacturer="Fujifilm", frame_size="35x24", memory_cards=nil>
<struct Camera manufacturer="Nikon", frame_size=:full_frame, memory_cards=nil>
<struct Camera manufacturer="Nikon", frame_size="23.6x15.6", memory_cards=nil>
<struct Camera manufacturer="Fujifilm", frame_size="23.6x15.6", memory_cards=nil>
<struct Camera manufacturer="Kodak", frame_size="35x24", memory_cards=[#<struct MemoryCard storage_capacity="32GB">]>
puts FactoryBot.build(:camera, :nikon, :full_frame, :with_memory_card, number_of_cards: 2).inspect
<struct Camera manufacturer="Nikon", frame_size="35x24", memory_cards=[#<struct MemoryCard storage_capacity="32GB">, #<struct MemoryCard storage_capacity="32GB">]>
```
