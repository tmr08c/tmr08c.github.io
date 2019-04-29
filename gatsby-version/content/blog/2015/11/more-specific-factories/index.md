---
title:  "Creating More Specific Factories"
date:   '2015-11-17T22:53:02.123Z'
categories: ['testing', 'ruby']
---

As I have discussed in [previous posts]({% post_url 2015-11-08-using-vim-to-drive-tdd %}) I have been working to adopt TDD into my workflow. One of the ways I have aimed to improve my testing skills is to better utilize the testing tools I am using. One common testing tools in the Rails space is [Factory Girl](https://github.com/thoughtbot/factory_girl). Factory Girl provides an easy way to create sample records of models from your application in your specs.

Factories are great because you can set them up with common base attributes and then you only have to specify the attributes you are worried about when you create the object. For example, we can have a `Person` factory that looks like:

```ruby
# factories.rb

factory :person do
  name 'Jane Doe'
  sex 'female'
  age 18
end
```

I could then create a person in my specs by doing the following:

```ruby
# person_spec.rb

it 'should return a count of the number of people' do
  FactoryGirl.create(:person)

  expect(Person.count).to eq 1
end
```

In this simple example we didn't have to clutter the spec with fake information for creating the `Person` when all we wanted was a record in the database.

As mentioned above, we can be more specific, if needed. For example, let's say we were getting a count of all of the male `Person` records.

```ruby
# person_spec.rb

it 'should return a count of only male users' do
  FactoryGirl.create(:person, sex: 'male')
  FactoryGirl.create(:person, sex: 'female')

  expect(Person.number_of_men).to eq 1
end
```

Above we were able to specify the value of only the attribute we were concerned with, the `sex`. You may notice that I specified the `sex` as female even though we have that as the default value in the factory we set up above. While this isn't necessary, I find that it makes the test less ambiguous and more resistant to change. Consider the following alternative:

```ruby
# person_spec.rb

it 'should return a count of only male users' do
  FactoryGirl.create(:person, sex: 'male')
  FactoryGirl.create(:person)

  expect(Person.number_of_men).to eq 1
end
```

This assertion would work the same as the previous. However, would you easily know what is happening when you come back to look at it weeks later? What if a coworker is writing a bunch of specs that need the `Person` records to be males and decides to update the factory's default `sex` attribute to be male? This would cause your spec to fail for what may not be a perfectly apparent reason.

This recent way of thinking about writing more verbose specs is actually what has really drawn me to creating more specific factories. Previously, I would have done what I showed in the second example, thinking less code is better code. Lately, however, I found found that specs often benefit from **not** always staying DRY.

Luckily, with FactoryGirl, there is a solution that is both DRY and more verbose, named factories, or [inheritance](https://github.com/thoughtbot/factory_girl/blob/master/GETTING_STARTED.md#inheritance) as the documentation refers to them.

Here is an example of a way we can use inheritance to make our factories more specific

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

which will lead to more clear specs.

The `female_person` and `male_person` will inherit all attributes from the parent factory and will override or set whatever attributes are specified, in this case `sex`.

We can now write the previous assertion more succinctly:

```ruby
# person_spec.rb

it 'should return a count of only male users' do
  FactoryGirl.create(:male_person)
  FactoryGirl.create(:female_person)

  expect(Person.number_of_men).to eq 1
end
```

In my opinion this example more directly describes what we are testing, is more resistant to change, and is even shorter.

This just touches on one aspect of FactoryGirl and in no way does this great gem justice, but is a small aspect that I have been utilizing in my specs lately that I have found to be tremendously helpful.
