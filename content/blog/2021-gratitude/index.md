---
title: "2021 Gratitude"
date: 2021-12-31 12:20:02
categories: ["personal", "scripting"]
---

Following the popular advice, I have been regularly listing things I am grateful for (or excited about). For most of 2021, I have been using the [dailies](https://www.orgroam.com/manual.html#Org_002droam-Dailies) functionality provided by [org-roam](https://www.orgroam.com/) as my digital storage place of gratitude. A draw to using a digital, text-based solution for something like this is the ability to own my data and easily process it if I wanted; after all, everything I write is in text files on my machine, right?

In practice, I rarely look back at my daily entries and have never done any bulk processing or aggregate analysis. Since we are nearing the end of the year, I thought it could be interesting to use my notes spark some reflection on the past year (and give me a somewhat easy blog post topic to write about).

## First attempt - `Org Element`

As you may guess from the project name, `org-roam` it is based on the popular Emacs package, [Org Mode](https://orgmode.org/). Org Mode works with `.org` files which use a "highly flexible structured plain text file format." If you are familiar with [markdown](https://daringfireball.net/projects/markdown/) Org formatting should look familiar.

Because Org files can be structured (different heading levels, lists, etc.) and my daily files followed a template, I thought I would be able to lean on existing functionality from Org to extract the data I need.

If we look at my template below, we can see my gratitude list is under its own header titled, "Grateful or Excited About."

```org
#+title: %<%Y-%m-%d>
#+startup: showall

* Grateful or Excited About
#+begin_comment
What are 3 things I am grateful for or excited about.
#+end_comment

1.

* Planning [0/1]
#+BEGIN_COMMENT
Target 5 Items I would like to get done today
#+END_COMMENT

- [ ]

* Notes
** Standup
*** Prep

-

*** Notes
* Reflection
** How was today?
#+BEGIN_COMMENT
On a scale of 1-10, how was yesterday?
#+END_COMMENT

%?

** What could I change or improve?
** What did I learn?
** What did I initiate?
** Who did I help?
```

Since I only needed this one section, I thought I would be able to find existing Org-related functions to give me all content under a given heading (also known as a [subtree](https://emacs.stackexchange.com/questions/17370/emacs-what-is-a-subtree-in-org-mode-and-how-do-you-create-one) in Org).

While most Org functions, understandably, expect to be called from within an Org document itself, I was able to learn about the [Org Element API](https://orgmode.org/worg/dev/org-element-api.html) which allows you to generate and interact with an AST of your content.

Unfortunately, after a few hours of faking my way through `elisp` writing, I was unable to get something that worked. I am including my work-in-progress with the hopes an emacs wizard can grace me with some knowledge (or I can use it as a starting point in the future).

```elisp
(defun tr/extract-gratitude ()
  "WIP Function to extract gratitude section from org-dailies"
  ; make it so I can run the function using M-x
  (interactive)
  ; out putput into a temp buffer for debugging
  (with-output-to-temp-buffer "*debug org fun*"
    ; get org AST for current buffer, only looking for headlines
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        ; filter out headlines for the one that matches what we want
        (if (string= (org-element-property :raw-value headline) "Grateful or Excited About")
          ; for our matching header, get the list of items we are gateful for
          (org-element-map (org-element-contents headline) 'plain-list
            (lambda (list)
              ; Seems like it shouldn't be necessary, but I was seeing content
              ; not nested under my header, so I tried matching again
              (if (eq (org-element-property :parent list) headline)
                (print (org-element-contents list))
                ; I wanted to get the content from the list, tried
                ; mapping over each "item" in the list
                (org-element-map (org-element-contents list) 'item
                  (lambda (item)
                    (print (org-element-contents item))))))))))))
```

Not finding a straightforward "extract data from this subtree" function was a bit surprising. Since people do some amazing things with Org Mode, I am going to assume this is a limitation of my familiarity with Org and `elisp` in general.

## Second Attempt - `sed`

After spending more time than I would have liked attempting the Org Elements solution, I decided to explore an option suggested in [this Reddit thread](https://www.reddit.com/r/emacs/comments/n1cqlw/parse_org_files_for_header_and_email_content_like/) where the poster wanted to pull content listed under the "lessons" header of their daily org journal files. [This commentor](https://www.reddit.com/r/emacs/comments/n1cqlw/comment/gwcbc5l/?utm_source=share&utm_medium=web2x&context=3) suggested getting outside of `elsip` for this task:

> That sounds like a job for an outside script (shell, Python, etc.). A simple sed / awk script will pull the text out of your org files, for example:
>
> sed -n '/^\* Lessons/,/^\*/p' dailyfiles\*.org

The suggestion was to use `sed` to pull out the data needed. Once I had the list, I could do whatever I wanted with it! Using their suggestion as a base, I eventually Googled my way the following `sed` command.

```bash
sed -n -E \
'/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' \
2021-*.org
```

There is a lot going on here, so let's break it down.

- The `-n` tells `sed` not to echo out each line it reads (we will print our matches later).
- The `-E` flag lets us use extended regular expressions; this allows you to not escape special characters you may be used to with more modern RegEx engines (in particular, I added it when I started using match groups).
- Within our match `//`, we have two regular expressions, joined by a comma (`,`). The comma syntax tells `sed` to match on everything starting with the first regular expression, up to (and including) the second. Our two regular expressions are (1) the heading that contains my gratitude entries and (2) any following headline (a line starting with one or more `*`). The result would look like the following:

```org
* Grateful or Excited About
#+begin_comment
What are 3 things I am grateful for or excited about.
#+end_comment

1. First thing
2. another thing
3. something else

* Planning [2/2]
```

This gets us in the right ballpark, but we still have content we don't need such as the headers and a comment.

- After our main search pattern (`//`), we have brackets with another command. `sed` allows you to [wrap commands between `{}` that act on the scoped output of the previous command](https://www.gnu.org/software/sed/manual/sed.html#Common-Commands). Acting on the matched output above, we match on lines that start with a digit (I am using an ordered list) and use regular expression groups to extract everything after the number (the actual text of what I am grateful for).
- Our second command ends with the `/p` flag. This tells `sed` to print the match. Paired with the `-n` flag above, the only output that should be printed is the matches from our regular expression.

After our regular expression, we use a [glob expression](<https://en.wikipedia.org/wiki/Glob_(programming)>) to get all org files in the current directly that start with `2021` and pass that as an argument to `sed`, having it parse all `org` daily files form this year.

## Processing the data

With less than 700 entries and the need to parse arbitrary text, I didn't think the [ROI for automation](https://xkcd.com/1205/) would work out in my favor, so I decided against attempting to script this part. Instead, I decided to manually process the entries by categorizing them.

```bash
› sed -n -E \
'/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' \
2021-*.org | \
wc -l

     671
```

To easy my manual processing, I decided to pipe the results into a CSV file and use Excel (well, actually Numbers).

```bash
sed -n -E \
'/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' \
2021-*.org > 2021-gratitude.csv
```

Despite being a single column, using the `csv` extension is a nice way to get my spreadsheet program to make things line up for me. I also know that I can use the same program to do things like generate visual representations of my data.

After going through the first few rows I identified my defined my basic categories and even identified some commonly used pharses for my entried. I was able to use a `LOOKUP` function combined with a match table to match some of the more common entries I had. The `LOOKUP` function and existing categories providing a decent starting point. I hand-reviewed each entry, re-categorizing as needed.

## Results

Here are aggregate results:

| Category                                      | Count |
| --------------------------------------------- | ----- |
| partner                                       | 162   |
| breaks / flexibility / ease of life / weekend | 96    |
| work                                          | 72    |
| life / lucky / fortune                        | 65    |
| morning routine / coffee                      | 50    |
| weather                                       | 31    |
| hoping for change in work                     | 27    |
| thing                                         | 23    |
| food                                          | 22    |
| financial health                              | 21    |
| physical health                               | 20    |
| travel / trips                                | 16    |
| pets                                          | 13    |
| friends / social                              | 12    |
| family                                        | 8     |
| mental health                                 | 7     |
| home                                          | 5     |
| nature                                        | 4     |
| career capital                                | 3     |
| nature                                        | 2     |
| outside of work programming things            | 2     |
| other                                         | 1     |
| educational resources                         | 1     |
| open source                                   | 1     |
| events                                        | 1     |

### Takeaways

I expected the aggregate of the data to be more useful than the individual, but, since I had to go through each record to categorize anyway I realized how impactful it was to read the actual entries. There were few enough entries that this was not a time-consuming task and I was able to notice trends without the need for computer-assistance. Reading the actual entries also triggered memories of what was going on at the time - from big events (job hunting, the passing of my dog) to small highlights (picking up croissants for breakfast, going out for a beer with a friend), I got the highlight reel of the past year.

Below are some of my major takeaways.

- Overusing answers

There were a number of go-to items on my list - my partner, being excited about an upcoming weekend (or grateful the past weekend was pleasant), and coffee were my top go-tos. While I am grateful for these things, I think I am over-using them as easy answers. I suspect there may be value in finding gratitude in a wider variety of things over the next year, especially if this means taking more time out of the day to think about what I am grateful for.

- Professional life

This year, I took on a new role to have the opportunity to work in [Elixir](https://elixir-lang.org/); something I have wanted to do for years! The result was multiple entries about being grateful for my new role, getting to work in Elixir, and Elixir (and the ecosystem) in general. I also had a few entries about my gratitude towards the recruiters that helped me learn about new opportunities. I know recruiters can sometimes get a bad rap, but I was able to learn about opportunities I would not have heard of otherwise and avoid the résumé inbox black hole. I 100% would not have my current job without the recruiter that I worked with.

My gratitude goes out not only to everyone that makes Elixir possible, but also to all open source maintainers across languages. Open source software is amazing and maintainers are incredible people that often put in countless hours for little to no recognition. I am curious if [GitHub Sponsors](https://github.com/sponsors) is the best way to show proper gratitude for maintainers.

- Conflicted gratitude

These last few years have been extremely difficult for much of the world. They have been especially difficult for those in marginalized groups. I am torn by my "life / lucky / fortune" category (and some of the answer in my "ease of life" category); I hope that it is a good thing I am more aware of the fortunes of my life and acknowledge my gratitude towards them, but I can't help but think about others that are not as fortunate as I am.

I think the next step for me here is to identify ways to leverage my good fortune in ways that can help others. I am unsure if this is monetary (like GitHub sponsors may be for open source), time, or something else.

To increase my chances of being grateful for my health and the life I lead, I also know I need to focus on maintaining (and improving) my physical and mental health through diet, exercise, meditation, and work-life balance.

## Conclusion

Simply reading through my gratitude entries has felt value. Despite taking the time to list what I am grateful for, I don't often think about what that means in the big picture. It has become another item on the TODO list that I simply want to check off. Reviewing the list not only helped me to take a step back and think about the past year, but it has also renewed my faith in the value of continuing this practice going forward.

In addition to the value of reflection, I also found it valuable from a technical perspective. I do not spend nearly enough time attempting to write code for one-off tasks like this and, when I do, I often choose a more known path. Exploring new areas (`elisp`, Org Element API) and failing to come up with a first-pass solution is a valuable practice in personal-time coding. It was also great to end up with a `sed`-based solution; `sed` is one of those Unix, command-line tools that are useful to have a basic knowledge of.
