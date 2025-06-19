---
date: '2020-04-10T10:21:22'
tags:
- ruby
- troubleshooting
title: Referencing a local gem in Gemfile
---

In a [recent post](/2020/04/how-i-debug-my-dependencies), I discussed my new debugging technique of pulling down third party libraries. While having the code local to read and search is a win in and of itself, _running_ the code can be even more helpful. Since this new practice is a habit I am still working to develop, I thought I would write down how to get it working. Hopefully, this [reduces the friction](https://twitter.com/jamesclear/status/1091687364953587714?lang=en) for my future self.

## What to do

Since our project is using [`Bundler`](https://bundler.io/), I was able to update the line where we add the gem in question to use the [`path`](https://bundler.io/man/gemfile.5.html#PATH) option and point it to the (relative) path on my system:

```ruby
# Gemfile
gem 'gem-im-debugging', path: '../gem-im-debugging'
```

Currently, I keep all repositories in the same directory. This enables me to use the relative path to the parent directory to find the local version of the gem I am debugging. Depending on where you copy the project,  you may need to adjust your path.

## Possible alternative

As I was trying to find the documentation to link to in this post, I also found [this option](https://bundler.io/v1.2/git.html#local):

```bash
bundle config local.GEM_NAME /path/to/local/git/repository
```

Below is the example from the docs getting it working for a local version of `rack`.

In your terminal set `Bundler` to use your local version of the gem:

```bash
bundle config local.rack ~/Work/git/rack
```

and then in your Gemfile, you can include the gem as though you are pulling it off of GitHub:

```ruby
# Gemfile
gem 'rack', :github => 'rack/rack', :branch => 'master'
```

My initial impression is that this could achieve the same thing, but seems to take a bit more work.

My guess is this would be useful if you regularly work on a gem and want to have some test projects around. Being able to reference different branches from the test project itself could be useful. Also, based on the documentation it seems `path` will not compile C extensions, but `git` (and `github`?) will. If you are working with C extensions you may have to leverage this alternative method.

As I find more opportunities to work with gems locally, I may want to revisit this option (and learn how to turn it back off). 

