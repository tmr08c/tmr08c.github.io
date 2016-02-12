---
layout: default
title: Blog
permalink: /blog/
---

<div class="blog">

  <ul class="post-list">
    {% for post in site.posts %}
      <li>
        <div class="post-link"><a href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a></div>
        <div class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</div>

      </li>
    {% endfor %}
  </ul>

  <p class="rss-subscribe">subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a></p>

</div>
