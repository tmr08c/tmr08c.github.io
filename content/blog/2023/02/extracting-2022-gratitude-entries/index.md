---
title: "Extracting 2022 Gratitude Entries"
date: 2023-02-28 20:00:00
categories: ["emacs-lisp"]
---

In my [last post](/2023/01/2022-gratitude), I reflected on the entries in my gratitude journal and my takeaways from the year. I had a [similar post in 2021](/2021/12/2021-gratitude/). In that post, I also included some details of how I got access to all of my entries, both how I [attempted](2021/12/2021-gratitude/#first-attempt---org-element) and how I [actually got it to work](/2021/12/2021-gratitude/#second-attempt---sed). In my 2021 post, I tried using the [Org Element API](https://orgmode.org/worg/dev/org-element-api.html), but ended up instead using `sed`. Wanting to put my `elisp` skills to the test a year later, I investigated the org-elements route again this year. This time, successfully.

For some background, I am using [`org-roam`'s dailies](https://www.orgroam.com/manual.html#org_002droam_002ddailies) feature as a place to write down three things for which I am grateful for or excited about each day. I enter them as an ordered list under the same header, something like:

```org-mode
\* Grateful or Excited About
#+begin_comment
What are 3 things I am grateful for or excited about.
#+end_comment

1. Thing 1
2. Thing 2
3. Thing 3
```

The goal was to find all of my `org-roam` dailies created in 2022, parse out the three items I listed, and combine them in a way to manually evaluate them. Here is what the final code ended up looking like: 

```emacs-lisp
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

At some point in the development, I decided to put the results into an `org-mode` table and include the date of the entry. This resulted in copying ~30 lines of code from [last time I worked with org-mode dates](/2022/08/add-timestamps-to-org-files/#getting-dates). In addition to the extra code, working with an `org-mode` table with a lot of entries could be slow at times. Since reviewing my gratitude entries is mostly a chance to walk down memory lane, having the dates provide enough value as an anchor point of context that it was worth it. However, I&rsquo;m calling out the extra work here to say they may not be worth it for a lot of cases.

`org-mode` date and timestamp management aside, there are two main aspects to the code: (1) the &ldquo;main&rdquo; function which manages finding the files and collecting the entries and (2) the `extract-gratitude-entries` function which uses `org-element` to extract my gratitude entries from an org file.

### The main function

The &ldquo;main&rdquo; function is our coordinator.

```emacs-lisp
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

-   We manage the buffer we write our results to, `*gratitude*`, including creating it (`generate-new-buffer`), resetting the contents if the buffer already has content (`set-buffer` and `erase-buffer`).
-   We find all daily files for the year using the `org-roam-directory` and `org-roam-dailies-directory` variables.
-   We format the gratitude entries as rows in an `org-mode` table and write them into our `*gratitude*` buffer, `(insert "|" date "|" entry "||\n")`.
-   We then switch to our output buffer, set it be an `org-mode` buffer, and align the table.

### `extract-gratitude-entries`

The `extract-gratitude-entries` function leverages the [Org Element API](https://orgmode.org/worg/dev/org-element-api.html) to parse our `org-mode` file (`org-element-parse-buffer`) in an AST and then traverse the AST to find our gratitude entries.

```emacs-lisp
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

The Org Element API provides the ability iterate over elements of a given type (`org-element-map`) and introspect individual nodes (`org-element-property`). With these two primary tools, our strategy is to:

1.  Look for our gratitude header
    We use `org-element-map` to fetch all headers and then check the value of the header (`(org-element-propery :raw-value)`) to see if it matches our header&rsquo;s text.
2.  Find the list of entries under the header
    Similar to our previous step, this is a combination of fetching all `plain-list` elements under our header and refining that list to look for our ordered list. In practice, I probably didn&rsquo;t have to do the extra check to ensure the item was an ordered list, since I only have my gratitude entries under the header.
3.  Get the contents of each item in the list
    Getting the content of the entries requires finding the items in the list and then for each item getting the contents. I struggled to get the contents in a useful format until [this Reddit comment](https://www.reddit.com/r/orgmode/comments/7qwmbo/comment/dstsmpw/?utm_source=reddit&utm_medium=web2x&context=3) brought `org-element-interpret-data` only my radar. This function will convert the Org Element AST structure into what would be displayed in the buffer.

Once we have collected our list of items we kill the buffer and return them.

####  Potential Element API Misuse

I don&rsquo;t know if this is an intended use of the Org Element API. It felt a bit too low-level for what I was trying to do.

While working on this two alternative approaches came to mind:

1.  Using headlines

    More of a &ldquo;maybe next time&rdquo; approach, `org-mode` seems to work well with quickly collecting and presenting headlines (Org Agenda, [org-ql](https://github.com/alphapapa/org-ql)). I suspect if I used sub-heading for my gratitude entries, I would have had a much easier time.
2.  Searching

    I am a novice emacs-lisper, but have noticed that examples do not shy away from opening buffers and searching for text. Similar to last year&rsquo;s `sed` approach, I wonder if I would have been better of searching for the proper header and yanking all text until the next header.

## Comparing with last year

### Code comparison

While our new version provides richer output by setting us up in an org-mode file and including the date of the entries, it come in at nearly 70 lines of emacs-lisp.

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

While the time spent working on the `emacs-lisp` version will pay dividends over my lifetime as an Emacs user, I think next year I will use the `sed` version again. As noted above, I suspect there are more Emacs-y ways to get this data in a way that would be more performance, but, at least as of right now, I think the time and effort in exploring those alternatives is better spent elsewhere.
