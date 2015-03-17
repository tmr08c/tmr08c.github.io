Cat = Struct.new(:type, :sex, :featured) do
  def to_s
    "#{sex}, #{type}"
  end
end


class CatSeller
  def newsletter
    %Q(
      We have some fantastic cats for sale this month!

      <h1>Featured Cats</h1>
      #{featured_cats.map { |cat| "<li>#{cat}</li>" }.join("\n")}

      <h1>All Cats</h1>
      #{cats.join(',')}
    )
  end

  def featured_cats
    cats.select! { |cat| cat.featured }
  end

  def cats
    @cats ||= fetch_cats
  end

  # We will pretend we are getting this from an API or DB
  def fetch_cats
    [
      Cat.new('Lion', 'Male', false),
      Cat.new('Leopard', 'Female', false),
      Cat.new('Snow Leopard', 'Male', true),
      Cat.new('Mountain Lion', 'Male', true),
      Cat.new('Tiger', 'Female', false),
    ]
  end
end

2.1.5 :229 > CatSeller.new.newsletter
 =>
 We have some fantastic cats for sale this month!

 <h1>Featured Cats</h1>
 <li>Male, Snow Leopard</li>
 <li>Male, Mountain Lion</li>

 <h1>All Cats</h1>
 <li>Male, Snow Leopard</li>
 <li>Male, Mountain Lion</li>

class CatSeller
  def featured_cats
    cats.select { |cat| cat.featured }
  end
end

 =>
We have some fantastic cats for sale this month!

<h1>Featured Cats</h1>
Male, Snow Leopard
Male, Mountain Lion

<h1>All Cats</h1>
<li>Male, Lion</li>
<li>Female, Leopard</li>
<li>Male, Snow Leopard</li>
<li>Male, Mountain Lion</li>
<li>Female, Tiger</li>
