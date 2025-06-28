---
date: '2023-02-28T20:00:00'
tags:
- emacs-lisp
title: Extracting 2022 Gratitude Entries
---

Following a [post from 2021](/2021/12/2021-gratitude/), my [last post](/2023/01/2022-gratitude) reflected on the entries in my gratitude journal and my takeaways from the year.

In the 2021 version of the same post, I included the details of my [attempt](/2021/12/2021-gratitude/#first-attempt---org-element) at using the [Org Element API](https://orgmode.org/worg/dev/org-element-api.html) as well as my ultimate [`sed`-based solutions](/2021/12/2021-gratitude/#second-attempt---sed). Wanting to evalute my `elisp` skills a year later, I attempted using the Org Elements API again; this post is the result of that investigation.

For some background, I use `org-roam`'s [dailies feature](https://www.orgroam.com/manual.html#org_002droam_002ddailies) as a place to write down three things for which I am grateful or excited about each day. I enter them as an ordered list under the same header, something like:

```org-mode
\* Grateful or Excited About
#+begin_comment
What are 3 things I am grateful for or excited about.
#+end_comment

1. Thing 1
2. Thing 2
3. Thing 3
```

To write my reflection post, I wanted to find all of my `org-roam` dailies created in 2022, parse out the three items I listed, and combine them in a way that allows me to review them together. Below is what the final code ended up looking like:

```el
(require 'org)
(require 'org-element)

(defun extract-gratitude-entries (file) "Get gratitude section of daily files."
       (find-file file)
       (let* (
              (lists (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (headline)
                         (and (string-equal (org-element-property :raw-value headline) "Grateful or Excited About")
                              (org-element-map headline 'plain-list
                                (lambda (list)
                                  (and
                                   (string-equal (org-element-property :type list) 'ordered)
                                   list)))))))
              (items (flatten-list (org-element-map lists 'item
                                     (lambda (item)
                                       (org-element-map (org-element-contents item) 'paragraph
                                         (lambda (p) (string-trim (org-element-interpret-data p)))))))))
          (kill-buffer)
          items))

 (defun tr/format-org-date (date)
   (org-timestamp-format date "\[%Y-%02m-%02d %3a %02H:%02M\]"))

(defun tr/time-stamp-to-org-timestamp (ts)
  "Taken from `org-timestamp-from-time` - the original function used
     `decode-time`, which doesn't work with our timestamp, so we use
     `parse-time-string` instead"
  (pcase-let ((`(,_ ,minute ,hour ,day ,month ,year . ,_) (parse-time-string ts)))
    (org-element-create 'timestamp
                        (list :type 'active
                              :year-start year
                              :month-start month
                              :day-start day
                              :hour-start hour
                              :minute-start minute))))

(defun tr/file-creation-time-from-name (fpath)
  "Extract a timestamp from the file name. Relies on files having the format
      'YYYYMMDDHHMMSS-*' (the default org-roam node filename)."
  (let ((filename (file-name-base fpath)))
    (tr/format-org-date
     (tr/time-stamp-to-org-timestamp
      (replace-regexp-in-string
       "\\([[:digit:]]\\{4\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)-.*"
       "\\1-\\2-\\3 \\4:\\5:\\6"
       filename)))))

(let ((output-buffer "*gratitude*")
      (files (directory-files (concat org-roam-directory org-roam-dailies-directory) 'full "2022.+\.org")))
  (split-window)
  (other-window 1)
  (generate-new-buffer output-buffer)
  (set-buffer output-buffer)
  (erase-buffer)
  (insert "|Date|Message|Category|\n|-|-|-|\n")
  (dolist (file files)
    (let ((date (tr/file-creation-time-from-name file)))
      (let ((entries (extract-gratitude-entries file)))
        (set-buffer output-buffer)
        (dolist (entry entries)
          (insert "|" date "|" entry "||\n")))))
  (switch-to-buffer output-buffer)
  (org-mode)
  (org-table-align))
```

There are two main aspects to the code:

1. The "main" function, which finds the files and collects the entries.
2. The `extract-gratitude-entries` function, which uses `org-element` to extract my gratitude entries from an org file.

### The main function

The &ldquo;main&rdquo; function is our coordinator.

```el
(let ((output-buffer "*gratitude*")
      (files (directory-files (concat org-roam-directory org-roam-dailies-directory) 'full "2022.+\.org")))
  (split-window)
  (other-window 1)
  (generate-new-buffer output-buffer)
  (set-buffer output-buffer)
  (erase-buffer)
  (insert "|Date|Message|Category|\n|-|-|-|\n")
  (dolist (file files)
    (let ((date (tr/file-creation-time-from-name file)))
      (let ((entries (extract-gratitude-entries file)))
        (set-buffer output-buffer)
        (dolist (entry entries)
          (insert "|" date "|" entry "||\n")))))
  (switch-to-buffer output-buffer)
  (org-mode)
  (org-table-align))
```

- We set up the buffer we write our results to, `*gratitude*`; creating it (`generate-new-buffer`) and resetting the contents if necessary (`set-buffer` and `erase-buffer`).
- We find all daily files for the year using the `org-roam-directory` and `org-roam-dailies-directory` variables.
- We format the gratitude entries as rows in an `org-mode` table and write them into our `*gratitude*` buffer, `(insert "|" date "|" entry "||\n")`.
- We then switch to our output buffer, set its major mode to `org-mode`, and align the table.

At some point during development, I decided to put the results into an `org-mode` table and include the date of the entry, resulting in copying ~30 lines of code from [last time I worked with org-mode dates](/2022/08/add-timestamps-to-org-files/#getting-dates).

In addition to the extra code, working with large-ish `org-mode` table can be slow when editing with auto-alignment enabled. While categorizing my entries, I add `#+STARTUP: noalign` to the buffer to work around this.

Since reviewing my gratitude entries is mostly a chance to walk down memory lane, having the dates provide enough value as an anchor point of context that it was worth it, but I'm calling out the extra work here to say they may not be worth it for a lot of cases.

### `extract-gratitude-entries`

As previously mentioned, the `extract-gratitude-entries` function leverages the Org Element API to parse our `org-mode` file (`org-element-parse-buffer`) into an AST and then traverse the AST to find our gratitude entries.

```el
(defun extract-gratitude-entries (file) "Get gratitude section of daily files."
       (find-file file)
       (let* (
              (lists (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (headline)
                         (and (string-equal (org-element-property :raw-value headline) "Grateful or Excited About")
                              (org-element-map headline 'plain-list
                                (lambda (list)
                                  (and
                                   (string-equal (org-element-property :type list) 'ordered)
                                   list)))))))
              (items (flatten-list (org-element-map lists 'item
                                     (lambda (item)
                                       (org-element-map (org-element-contents item) 'paragraph
                                         (lambda (p) (string-trim (org-element-interpret-data p)))))))))
         (kill-buffer)
         items))
```

The Org Element API provides functions to iterate over elements of a given type (`org-element-map`) and introspect individual nodes (`org-element-property`). With these two primary tools, our strategy is to:

1. Look for our gratitude header

   We use `org-element-map` to fetch all headers and then check the contents (`(org-element-property :raw-value)`) to see if it matches our gratitude header's text ("Grateful or Excited About").

2. Find the list of entries under the header

   Similar to our previous step, we first fetch, this time nested, elements of a type (`plain-list`) and then refining that list, time looking for ordered lists.

   In practice, I probably didn&rsquo;t have to do the extra check to ensure the item is an ordered list since I only have my gratitude entries under the header.

3. Get the contents of each item in the list

   I struggled to get the contents in a useful format until [this Reddit comment](https://www.reddit.com/r/orgmode/comments/7qwmbo/comment/dstsmpw/?utm_source=reddit&utm_medium=web2x&context=3) brought `org-element-interpret-data` only my radar. This function will convert the Org Element AST structure into what would be displayed in the buffer.

Once we have collected our list of items, we kill the buffer and return.

#### Potential Element API Misuse

I don&rsquo;t know if this is an intended use of the Org Element API; it felt a bit too low-level for what I was trying to do.

While working on this, two alternative approaches came to mind:

1. Using headlines

   `org-mode` seems to work well with quickly collecting and presenting headlines (Org Agenda, [org-ql](https://github.com/alphapapa/org-ql)). If I use headings instead of list items for my gratitude entries, I expect I could more easily collect the entries.

2. Searching

   I am a novice emacs-lisper, but I have noticed that examples do not shy away from opening buffers and searching for text. Similar to last year&rsquo;s `sed` approach, I wonder if I would have been better off searching for the proper heading and yanking all text until the one.

## Comparing with last year

### Code comparison

While our new version provides richer output by setting us up in an org-mode file and including the date of the entries, it comes in at nearly 70 lines of emacs-lisp.

Last year&rsquo;s `sed` version is a one-liner. While terse and requiring an understanding of `sed`, I think it would be easier to understand and change than our emacs-lisp version.

Winner: **sed**

### Performance

Using `benchmark-run`, our new emacs-version averages a run time of 48 seconds (35.688539, 47.857797, 61.151746).

Last year&rsquo;s `sed` version took less than a tenth of a second:

```bash
$ time \
sed -n -E \
'/^\*+ Grateful or Excited About/,/\*+/{ s/^[[:digit:]]\.[[:blank:]]*(.*)$/\1/p; }' \
2022-*.org > 2022-gratitude.csv

0.03s user 0.01s system 97% cpu 0.041 total
```

Winner: **sed**

## Takeaways

While the time spent working on the `emacs-lisp` version will pay dividends over my lifetime as an Emacs user, I will likely use the `sed` version next year. Unless I decide to use it as an opportunity to continue learning `emacs-lisp`, the `sed` version works quite well for my needs.
