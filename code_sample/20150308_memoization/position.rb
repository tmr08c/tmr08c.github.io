# example where you have to call multiple times
class TwitterFeed
  def display
    display_posts
    display_top_posters
  end

  def recent_posts
    # fetch recent posts from the API
  end

  def display_posts
    posts = recent_posts
    # logic to display posts
  end

  def display_top_posters
    posts = recent_posts
    # use post info to find most prolific posters
  end
end

# example where order matters
class TwitterFeed
  def display
    posts = recent_posts
    display_posts(posts)
    display_top_posters(posts)
  end

  def recent_posts
    # fetch recent posts from the API
  end

  def display_posts(posts)
    # logic to display posts
  end

  def display_top_posters(posts)
    # use post info to find most prolific posters
  end
end


# example with memoization
class TwitterFeed
  def display
    display_posts
    display_top_posters
  end

  def recent_posts
    # fetch recent posts from the API
    @recent_posts ||= Twitter.tweets
  end

  def display_posts
    # logic to display posts
  end

  def display_top_posters
    # use post info to find most prolific posters
  end
end
