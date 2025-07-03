#!/bin/bash

echo "Fixing broken links in Hugo site blog posts..."

cd hugo-site

# Fix CodeSchool and EnvyLabs links (missing https://)
find content/blog -name "*.md" -exec sed -i '' 's|\[CodeSchool\](www\.codeschool\.com)|[CodeSchool](https://web.archive.org/web/20190708190646/https://www.codeschool.com/)|g' {} \;
find content/blog -name "*.md" -exec sed -i '' 's|\[EnvyLabs\](www\.envylabs\.com)|[EnvyLabs](https://web.archive.org/web/20191220012018/https://www.envylabs.com/)|g' {} \;

# Fix StackOverflow links (change http to https)
find content/blog -name "*.md" -exec sed -i '' 's|http://stackoverflow\.com|https://stackoverflow.com|g' {} \;

# Fix Strange Leaflet link (404)
find content/blog -name "*.md" -exec sed -i '' 's|https://www.strangeleaflet.com/strange-leaflet-about-elixir-page1|https://web.archive.org/web/20220301000000*/https://www.strangeleaflet.com/strange-leaflet-about-elixir-page1|g' {} \;

# Fix David Allen quote link (404) if present
find content/blog -name "*.md" -exec sed -i '' 's|https://earnworthy.com/david-allen-quote/|https://www.goodreads.com/quotes/1397-the-key-is-not-to-prioritize-what-s-on-your-schedule|g' {} \;

# Fix Robin Sharma's book link (404) if present
find content/blog -name "*.md" -exec sed -i '' 's|https://www.robinsharma.com/book/the-5am-club|https://www.amazon.com/5AM-Club-Morning-Elevate-Life/dp/1443456624|g' {} \;

echo "Fixed broken external links in Hugo site."