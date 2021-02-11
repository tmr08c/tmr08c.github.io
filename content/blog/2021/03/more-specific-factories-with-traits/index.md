---
title: "Creating More Specific Factories with Traits"
date: '2021-03-20T06:14:13.265Z'
categories: ['testing', 'ruby']
---

In a [previous post](/2015/11/more-specific-factories), I wrote about creating more specific [FactoryBot](https://github.com/thoughtbot/factory_bot) factories using FactoryBot's [inheritance and nested factories](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#inheritance) capabilities. In this post, I will cover creating more specific factories leveraging [traits](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits) instead. 

Similar to inheritance, you can use traits to create additional "presets" for your factories. One advantage of traits, and the reason I find myself using them more often, is their composability. Instead of relying on everything being defined upfront in your nested factory, you can combine the pieces you need at test time to build the perfect factory.

## Defining Our Models

For our example factories, we will be working with two models: `Camera` and `MemoryCard` where a `Camera` can have zero [or more](https://www.howtogeek.com/392378/what’s-the-big-deal-about-dual-storage-card-slots-for-cameras/) `MemoryCard`s.

While you may be more familiar with using FactoryBot with Rails, it can be used to create factories for any Ruby class. For our `Camera` and `MemoryCard` classes, we are going to use simple Ruby [`Struct`s](https://ruby-doc.org/core/Struct.html):

```ruby
Camera =
  Struct.new(
    :manufacturer,
    :frame_size,
    :memory_cards
  )

MemoryCard =
  Struct.new(:storage_capacity)
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

With this in place, we can now easily create instances of our models without the need to specify attributes every time.

```ruby
FactoryBot.build(:camera)
=> <Camera
    manufacturer="Kodak",
    frame_size="35x24",
    memory_cards=nil>

FactoryBot.build(:memory_card)
=> <MemoryCard
    storage_capacity="32GB">
```

## Adding Traits

Our baseline factories provide an easy way to create new, valid instances of our models. If our test requires any different attributes, we can set them when creating our factory. For example, maybe we need to test how we calculate crop factor. Since the crop factor is a [calculation based on the frame size](https://shuttermuse.com/calculate-cameras-crop-factor/), we could create factories with various frame sizes:

```ruby
describe '#crop_factor' do
  context 'when working with a full-frame camera' do
    let(:camera) do
      FactoryBot.build(:camera, frame_size: '35x24')
    end
  end

  context 'when working with a APS-C camera' do
    let(:camera) do
      FactoryBot.build(:camera, frame_size: '6.17x4.55')
    end
  end
end
```

If we find we are regularly setting `frame_size` to a few standard dimensions, we may consider defining some traits.

The syntax for creating a trait is similar to creating a factory. We have a `trait` block that we use to name our trait, and, within the trait block, we use the same syntax to set values for our attributes. Below, we have defined traits that represent standard sensor types, specifically, their frame sizes.

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

In our test, we can now use these traits when creating new `Camera` records. Using factories this way allows us to work with standard sensor sizes without knowing the exact dimensions.

```ruby
# use trait and baseline attribute
FactoryBot.build(:camera, :aps_c)
=> <Camera
  manufacturer="Kodak",
  frame_size="23.6x15.6",
  memory_cards=nil>

# use trait and specify attributes
FactoryBot.build(
  :camera,
  :full_frame,
  manufacturer: "Nikon"
)
=> <Camera
    manufacturer="Nikon",
    frame_size="35x24",
    memory_cards=nil>
```

As you can see above, traits will still inherit the default values set up in our baseline factory, and we are still able to override any individual attributes.

Now that we have introduced traits, I want to mention that, for this example, it _could_ make sense to set `frame_size` directly when building our factories. Since `crop_factor` is a mathematical formula based on `frame_size`, it may be easier to understand our test expectations if we see the actual `frame_size` (as opposed to it potentially being obfuscated by our trait). This concern reveals some of the subtly of dealing with traits and the potential to introduce [mystery guests](https://thoughtbot.com/blog/mystery-guest). For a casual photographer like myself, the relationship between `frame_size` and `crop_factor` is not something I understand well.  In my case, having the `frame_size` set directly in the test could provide a signal of how it is involved in the `crop_factor` calculation. On the other hand, you will likely find that an experienced team working on this application will have industry expertise and know `full_frame` means  35x24 and how that impacts the `crop_factor` calculation. These concerns are a balance you will want to experiment with as your application, test suite, and team evolves. 

## Combining Traits

One of the main reasons I find myself reaching for traits over inheritance is the ability to combine traits when creating new factories.

To try out combining traits, imagine that we need to support searching for a camera by any of its attributes. Starting with a single attribute, we can use our existing trait for `frame_size` and write a test like the following.

```ruby
describe '#search' do
  context 'when searching by frame size' do
    let(:full_frame_camera) do
      build(:camera, :full_frame)
    end

    let(:aps_c_camera) do
      build(:camera, :aps_c)
    end
  end
end
```

Our current context only deals with searching for a single attribute, but we want our search ability to handle filtering on multiple attributes at once. To test this, we can try searching by `frame_size` **and** `manufacturer`. Before we write the test, we will create a few traits for `manufacturer`s.

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
=> <Camera
    manufacturer="Fujifilm",
    frame_size="23.6x15.6",
    memory_cards=nil>

FactoryBot.build(:camera, :nikon, :full_frame)
=> <Camera
    manufacturer="Nikon",
    frame_size="35x24",
    memory_cards=nil>
```

While this is equivalent to specifying the `frame_size` and `manufacturer` attributes directly, with well-named traits you can quickly see what is being set without specifying every attribute. The ability to mix-and-match traits also makes it easy for us to build up more varied combinations in our tests.

```ruby
describe '#search' do
  context 'when searching by multiple attributes' do
    let(:full_frame_nikon) do
      build(:camera, :full_frame, :nikon)
    end

    let(:aps_c_nikon) do
      build(:camera, :aps_c, :nikon)
    end

    let(:aps_c_fujifilm) do
      build(:camera, :aps_c, :fujifilm)
    end
  end
end
```

## Working with Associations

Without traits, if we wanted our camera to include a memory card, we would use FactoryBot to create a `MemoryCard` and pass it in when building out the camera.

```ruby
context 'when a camera has a memory card' do
  let(:memory_card) do
    build(:memory_card)
  end

  let(:camera) do
    build(:camera, memory_cards: [memory_card])
  end
end
```

Creating a `MemoryCard` provides us with the flexibility to customize the `memory_cards` we supply to our camera record. However, we may find we often do not care about the `storage_capacity` of the memory card in the camera, but rather just that the camera _has_ a memory card. In these cases, explicitly creating a `MemoryCard` record in our test could add unnecessary noise and be more easily managed with a trait.

We can create a trait on our `Camera` model that indicates this camera includes a `MemoryCard`.

```ruby
factory :camera do
  trait :with_memory_card do
    memory_cards { [build(:memory_card)] }
  end
end
```

Our trait sets our `memory_cards` association to be a single-element array with a factory-built `MemoryCard`. We can simply `build` our association because we are working with `Struct`s and not `ActiveRecord`-backed models. If you are in a Rails app, you will want to review the different options for [specifying an association](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#associations) with FactoryBot and decide which makes sense for your situation.

```ruby
FactoryBot.build(:camera, :with_memory_card)
=> <struct Camera
          manufacturer="Kodak",
          frame_size="35x24",
          memory_cards=[
            <struct MemoryCard storage_capacity="32GB">
          ]>
```

We can now update our example above to no longer explicitly create the memory card.

```ruby
context 'when a camera has a memory card' do
  let(:camera) { build(:camera, :with_memory_card) }
end
```

### Leveraging Transient Attributes

We can go a step further with our trait and add the ability to specify how many memory cards a `Camera` has. For this, we can use [transient attributes](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#transient-attributes). Transient attributes allow you to pass variables into FactoryBot that are not a part of the model you are creating but will be referenceable during the creation of the factory instance.

The syntax for transient attributes is very similar to the syntax of setting attributes on a factory.

```ruby
factory :camera do
  trait :with_memory_card do
    transient do
      number_of_cards { 1 }
    end

    memory_cards do
      Array.new(number_of_cards) { build(:memory_card) }
    end
  end
end
```

Here, we have a `transient` block that sets up an attribute, `number_of_cards` and defaults it to `1`. Now, when we create our `memory_cards` associations, we create an `Array` with `number_of_cards` elements where each element defaults to a new, factory-built instance of a `MemoryCard`.

Just like we can explicitly set attributes when creating a factory, we can explicitly set our transient attributes as well.

```ruby
# use default value for `number_of_cards`
FactoryBot.build(
  :camera,
  :nikon,
  :full_frame,
  :with_memory_card
)
=> <Camera
    manufacturer="Nikon",
    frame_size="35x24",
    memory_cards=[
      <MemoryCard storage_capacity="32GB">
    ]>

# set value for `number_of_cards`
FactoryBot.build(
  :camera,
  :nikon,
  :full_frame,
  :with_memory_card,
  number_of_cards: 2
)
=> <Camera
    manufacturer="Nikon",
    frame_size="35x24",
    memory_cards=[
      <MemoryCard storage_capacity="32GB">,
      <MemoryCard storage_capacity="32GB">
    ]>
```

This pattern is great for tests that interact with associated records and don't require any special setup for the associated objects.

```ruby
describe '#store_picture' do
  context 'when there are multiple memory cards' do
    let(:camera) do
      FactoryBot.build(
        :camera,
        :with_memory_card,
        number_of_cards: 2
      )
    end
  end
end
```

## Inheritance is still on the table

While I find I don't often reach for the option of inheritance anymore, using traits does not prevent you from using this feature. Rather, you can build nested factories that leverage your traits.

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
=> <Camera
    manufacturer="Kodak",
    frame_size="35x24",
    memory_cards=[
      <MemoryCard storage_capacity="32GB">,
      <MemoryCard storage_capacity="32GB">
    ]>

FactoryBot.build(:prosumer_camera)
=>  <Camera
      manufacturer="Sony",
      frame_size="23.6x15.6",
      memory_cards=[
        <MemoryCard storage_capacity="32GB">
      ]>
```

By creating traits, you can use the same tools when defining new factories or when building custom records in your tests.

## Conclusion

Traits are a go-to option for me when working with FactoryBot because they provide easy to use building blocks for building out custom models. While you won't set every attribute via trait, hopefully, this post helped show you when traits can be useful.

In this post, we covered how to leverage FactoryBot traits for setting attributes and managing associations. As you begin to use traits in your projects, I would recommend reading through the [full documentation](https://github.com/thoughtbot/factory_bot/blob/master/GETTING_STARTED.md#traits) as it covers additional use cases.

