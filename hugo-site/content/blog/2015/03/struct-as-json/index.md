---
date: '2015-03-01T21:11:02'
tags:
- ruby
title: Return Struct as JSON
---

I am working on a side project in which I want to use [Sinatra](http://www.sinatrarb.com/) as a simple backend and experiment with a more rich Javascript front end.

## The Setup

The idea was to respond to a `GET` request to my endpoint with a JSON-ified array of simple Ruby [Structs](http://ruby-doc.org/core-2.2.0/Struct.html).

The route looked something like

```ruby
get '/api/v1/tracks' do
  track_lister = TackLister.new
  track_lister.tracks.to_json
end
```

the call to the `TrackLister`'s `tracks` was to return an array of `Track` structs.

```ruby
class TrackLister
  Track = Struct.new(:name, :artist)

  def tracks
    [].tap do |tracks|
      2.times { |i| tracks << Track.new("Track #{i}", "Band #{i}") }
    end
  end
end
```

firing up `irb` this seems to have worked well enough

```ruby
>> TrackLister.new.tracks
#=> [
  #<struct TrackLister::Track name="Track 0", artist="Band 0">,
  #<struct TrackLister::Track name="Track 1", artist="Band 1">
]
```

## The Problem

The `to_json` version of my response looks a little funny

```ruby
>> require 'json' #=> true
>> TrackLister.new.tracks.to_json
#=> "[\"#<struct TrackLister::Track name=\\\"Track 0\\\", artist=\\\"Band 0\\\">\",\"#<struct TrackLister::Track name=\\\"Track 1\\\", artist=\\\"Band 1\\\">\"]"
```

Since this was supposed to be a response Javascript would need to parse I figured I would check to see how Javascript would handle it:

```javascript
>> JSON.parse(my_long_json_string)
=> [
  "#<struct TrackLister::Track name="Track 0", artist="Band 0">",
  "#<struct TrackLister::Track name="Track 1", artist="Band 1">"
]
```

It can parse the JSON but it results in an array of strings and there seemed to be no easy way for me to get to the data

```javascript
>> tracks[0]
=> "#<struct TrackLister::Track name="Track 0", artist="Band 0">"
>> tracks[0]['name']
=> undefined
>> tracks[0].name
=> undefined
>> JSON.parse(tracks[0])
=> Uncaught SyntaxError: Unexpected identifier
```

It seemed that the JSON version of my structs was not being deserialized the way I would have liked.

## The Solution

The solution was to serialize my data in a way that Javascript would like and could easily deserialize - a `Hash`.

With the [addition of `to_h` in Ruby 2](http://www.benjaminoakes.com/2013/03/08/all-about-to_h-in-ruby-2/) converting my `Track` struct into a `Hash` was very easy:

```ruby
>> TrackLister.new.tracks.map(&:to_h)
#=> [{:name=>"Track 0", :artist=>"Band 0"}, {:name=>"Track 1", :artist=>"Band 1"}]
```

this could then be converted to JSON just like before

```ruby
>> TrackLister.new.tracks.map(&:to_h).to_json
#=> "[{\"name\":\"Track 0\",\"artist\":\"Band 0\"},{\"name\":\"Track 1\",\"artist\":\"Band 1\"}]"
```

Now when test out this new JSON string in Javascript's `JSON.parse` I end up with Javascript objects

```javascript
>> tracks = JSON.parse(my_long_json_string)
=> [Object, Object]
```

which allow me to easily access my data's attributes

```javascript
>> tracks[0]
=> Object {name: "Track 0", artist: "Band 0"}
>> tracks[0].name
=> "Track 0"
>> tracks[0].artist
=> "Band 0"
```
