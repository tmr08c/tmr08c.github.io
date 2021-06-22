---
title: "Changed Opinions: Comments"
date: "2021-07-28T07:01:13.265Z"
categories: ["software development"]
---

Over my time as a developer my thoughts, opinions, styles, and preferences have continued to grow, form, and often change. One area I have found my practices have shifted is with my use and preferences around comments.

In my introduction to programming course, we were told to add comments to every function we wrote. Literally. Being new to programming and struggling to get simple `for` loops to work, I expect most of the comments I added were mostly useless. A common problem I believe school projects is have is how temporary they are. Most projects I had were worked on only by me, for a few weeks, and never revisited. With this style of project, I rarely found value in comments. As a result, as the tyranny of required comments was lifted, I found the habit of using them didn't keep up.

Fast forward to me joining an internship and getting to write Ruby. When learning about Ruby, I also learned about the concept of self-documenting code. The idea behind self-documenting code is you want to favor writing code in a style that is easy to read and does not require line-by-line comments explaining what is going on. An example of self-documenting code is creating desciptively named variables and functions.

The book, [_Eloquent Ruby_](http://eloquentruby.com/) sticks out in my mind has helping me to learn about this idea. From the section, ["Go Easy on the Comments"](https://www.google.com/books/edition/Eloquent_Ruby/-s2xL0pVsLUC?hl=en&gbpv=1&dq=eloquent%20ruby&pg=PA6&printsec=frontcover&bsq=go%20easy%20on%20the%20comments).

> Good Ruby code should speak for itself, and most of the time you should let it do its own talking

My early understanding of this concept led me to believe comments were a code smell - you were unable to write code that was understandable on its own and had to fall back to prose. While my opinons around code comments have changed, I think starting with this extreme belief helped improve my code. By working under the belief that my code had to be understanable on its own, I made great efforts to make that possible. Similar to writing a program or solving a kata [under some restriction](https://www.youtube.com/watch?v=TKFi3f_W33k&list=PL0zVEGEvSaeHgdv6t242ukQNIOtHNgzOJ&index=5) to push a concept to the extreme, trying to write code without comments helped me understand the bounds of self-documenting code.

It's fortuitous that Google Books has this section of the book available, because it reveals a fault in my memories. My memory of _Eloquent Ruby_ was a hardline stance on avoiding comments. However, reading on the book goes on to include reasons **for** including comments.

> There are good reasons for adding comments to your code, the best being to
> explain how to use your software masterpiece. These kinds of "how to" comments
> should focus on exactly that: how to use the thing. Don't explain why you
> wrote it, the algorithm that it uses, or how you got it to run faster than
> fast. Just tell me how to use the thing and remember that examples are always
> welcome

The book even continues on with additional exceptions of when it makes sense to add even why and how comments.

Similar to how my early programming years were an experiment in sparingly writing comments, I've spent the last year experimenting with being more liberal. A contributing factor to my increased commentary was due to my team's usage of [YARD](https://yardoc.org/), a Ruby documentation tool, and [Solargraph](https://solargraph.org/), a Ruby [language server](https://microsoft.github.io/language-server-protocol/).

With YARD we strove to include [`@param`](https://rubydoc.info/gems/yard/file/docs/Tags.md#param) and [`@return`](https://rubydoc.info/gems/yard/file/docs/Tags.md#return) tags to provide type annotation to our methods. These type annotations worked with Solargraph to provide documentation to provide _some_ type sanity to the dynamically typed Ruby. I found that since I was already adding comments to annotate the types related to my method, I would often see if there were other useful [YARD tags](https://rubydoc.info/gems/yard/file/docs/Tags.md) or information I could add. Over time, this developed into a handful of common types of comments I add to my programs.

- Will often push for "why" comments versus "what" something is doing comments. Sometimes "how" makes sense
- Brief introductin to method
  - weakest useage - this can often redudnant when well named
  - can be nice in Yard docs
  - often use as a starting sentence when I know I will be writing more
- Class-level docs
  - could cover why a class exists
  - may cover how the class works at a high level (try to think of simple example like ComplianceSet)
- TODO
  - TODO + ticket number to easily find
- Context

  - Known limitations
  -

* http://eloquentruby.com/
* https://www.google.com/books/edition/Eloquent_Ruby/-s2xL0pVsLUC?hl=en&gbpv=1&dq=eloquent%20ruby&pg=PA6&printsec=frontcover&bsq=eloquent%20ruby
