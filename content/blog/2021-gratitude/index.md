---
title: "2021 Gratitude"
date: 2021-12-31 12:20:02
categories: ["personal", "scripting"]
---

Following current popular advice, I have been regularly prompting myself to list things I am grateful (or excited about). I have been doing this somewhat regularly in a few different forms over the past few years. Most recently, I have been using the [dailies](https://www.orgroam.com/manual.html#Org_002droam-Dailies) functionality provided by [org-roam](https://www.orgroam.com/). With dailies, I provide a template of what I want to fill out each day. A draw to using a digital, text-based solution for something like this is the ability to own my data and easily process it if I wanted; afterall, it's all in text files on my machine, right?

In practice, I rarely look back and my daily entries (sometimes I can find useful meeting notes) and have never done any bulk processing or aggregate analysis. Since we are nearing the end of the year, I thought it could be interesting to use my notes spark some reflection on the past year (and give me a somewhat easy blog post topic to write about).

There are two main sections in my dailies template that could be useful for reflection analysis:

1. List of three things I am grateful for or excited about
2. Listing how I felt about the day on a scale of 1-10

While the second point is probably more interesting for trend analysis, I decided to start with the first because I expected it to be easier because I knew I would have more reliable formatting.

## First attempt - `Org Element`

As you may expect from the project names `org-roam`, it is based on the popular Emacs package, [Org Mode](https://orgmode.org/). Org Mode works with `.org` files which use a "highly flexible structured plain text file format." If you are familiar with [markdown](https://daringfireball.net/projects/markdown/) Org formating will feel familiar.

Because Org files can be structured and my daily files followed a template, I thought I would be able to lean on existing functionality from Org to extract the data I need.

If we look at my template below, we can see my gratitude list is under its own header titles "Grateful or Excited About."

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

Since I only needed this one section, I thought I would be able to find existing Org-related functions to give me all content under a given headeing (known as a [subtree](https://emacs.stackexchange.com/questions/17370/emacs-what-is-a-subtree-in-org-mode-and-how-do-you-create-one) in Org).

While most Org functions, understandably, expect to be called from within an Org document itself, I was able to learn about the [Org Element API](https://orgmode.org/worg/dev/org-element-api.html).

Unfortunately, after a few hours of faking my way through elisp writing, I was unable to get something that worked. I am including my work-in-progress with the hopes an emacs wizard can grace me with some knowledge (or I can use it as a starting point in the future).

```elsip
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
                         (print (org-element-contents item)))))))
          )))))

```

Not finding a straightforward "extract data from ths subtree" function was a bit surprising. Since people do some amzing things with Org Mode, I am going to assume this is a limitation of my familiarity with Org and elisp in general. It does also seem like Org Mode is more focused on flexibility when working within an Org file and less focused on providing general batch processing and parsing tools. This makes sense, especially since there are so may tools in the Unix world for working with text files, which brings us to my second attempt.

## Second Attempt - `sed`

After exceeding the time limit I set for myself to get this data, I decided to explore an option suggested in [this Reddit thread](https://www.reddit.com/r/emacs/comments/n1cqlw/parse_org_files_for_header_and_email_content_like/). Similar to what I was trying to do, the original poster wanted to pull content listed under the "lessons" header of their daily org journal files.

[This Reddit commentor](https://www.reddit.com/r/emacs/comments/n1cqlw/comment/gwcbc5l/?utm_source=share&utm_medium=web2x&context=3), has a suggestion outside of Emacs and Org, that seemed promising:

> That sounds like a job for an outside script (shell, Python, etc.). A simple sed / awk script will pull the text out of your org files, for example:
>
> sed -n '/^\* Lessons/,/^\*/p' dailyfiles\*.org

The suggestion was to use `sed` to pull out the data needed. Once I had the list, I could do whatever I wanted with it! Using their suggestion as a based, I eventually Googled my way into:

```bash
sed -n -E '/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' 2021-*.org
```

Let's break this down.

- The `-n` tells `sed` not to echo out each line it reads
- The `-E` flag lets us use extended regular expressions; this allows you to not escape special characters you may be used to with more moden RegEx engines (e.g., `()` for matching).
- Within our match `//`, we have two regular expressions, joined by a comma (`,`). The comma syntax tells `sed` to match on everything starting with the first regular expression, up to (and including) the second. Our two regular expressions are (1) the headling the contains my gratitude entries and (2) any following headline (one or more `\*``). The result would look like the following

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

As you can see, this gets us in the right ballpark, but we still have quite a bit of content we don't need such as the headers and a comment.

- After our main search pattern (`//`), we have brackets with another command. `sed` allows you to [wrap commands between `{}` that act on the scoped output of the previous command](https://www.gnu.org/software/sed/manual/sed.html#Common-Commands). Acting on the matched output above, we match on lines that start with a digit (I am using an ordered list) and use regular expression groups to extract everything after the number.
- Our second command ends with the `/p` flag. This tells `sed` to print the match. Paired with the `-n` flag above, the only output that should be printed is the result of our `sed` commands.

We use a glob expression to get all org files in the current directly that start with `2021` and pass that as an argument to `sed`, having it parse all `org` daily files form this year.

## Processing the data

I decided against trying to be too clever when parsing the data and manage it (mostly) by hand.

```bash
› sed -n -E '/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' 2021-*.org | wc -l
     671
```

With less than 700 entries and the need to parse arbitrary text, I didn't think the [ROI for automation](https://xkcd.com/1205/) would work out in my favor.

Instead, I decided to pipe the results in the file and use Excel (well, actually Numbers), to help me process the data.

```bash
sed -n -E '/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' 2021-*.org > 2021-gratitude.csv
```

Despite being a single column, using the `csv` extension is a nice way to get my spreadsheet program to make things line up for me. I also know that I can use the same program to do things like generate visual representations of my data.

Because my entries can be entered free-hand, I thought an interesting form of processing could be to categorize my entries. After going through the first few rows I develop a few categories. I then used a `LOOKUP` function in `Nummbers` and a match table to match some of the more common entries I had.
