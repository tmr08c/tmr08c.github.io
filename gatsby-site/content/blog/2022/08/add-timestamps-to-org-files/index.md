---
title: "Add Timestamps to org-roam Files"
date: "2022-08-31T18:08:13.265Z"
categories: ["emacs", "org-roam", "emacs-lisp", "note-taking"]
---

In this post, I cover adding timestamps to track creation and modification times in [`org-roam`](https://github.com/org-roam/org-roam) files and how to update existing nodes to include these timestamps.

## Adding the timestamps

We will track our creation and modification timestamps with [keywords](https://orgmode.org/worg/dev/org-syntax.html#Keywords). This post uses `created_at` and `last_modified`, but you can choose alternatives as you see fit.

We will begin by ensuring newly created nodes include our timestamp fields. We do this by updating our capture templates.

```el
(setq org-roam-capture-templates
  '(("d" "default" plain "%?" :target
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created_at: %U\n#+last_modified: %U\n\n\n")
  :unnarrowed t)))
```

This template is based on the [default in org-roam](https://github.com/org-roam/org-roam/blob/c3867619147175faf89ed8f3e90a1e67a4fd9655/org-roam-capture.el#L41-L45), and simply adds our two new keywords.

```el {diff}
'(("d" "default" plain "%?" :target
-   (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
+   (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created_at: %U\n#+last_modified: %U\n\n\n")
    :unnarrowed t))
```

If you have multiple capture templates, you will need to update them all.

### Backfilling Data

At this point, **new** `org-roam` files will include the `created_at` and `last_modified` keywords . This section covers updating all of our existing files to include these keywords as well.

_Please note that I used this as a chance to learn some `emacs-lisp`, so it may not be following best practices. Only after the fact did I realize I should have referenced something like [org-roam-migrate.el](https://github.com/org-roam/org-roam/blob/main/org-roam-migrate.el) for a more robust solution. That said, it does work which isn't bad for one-time-use code._

### Getting Dates

Since I am leveraging `org-roam`'s [default filename capture template](https://github.com/org-roam/org-roam/blob/7f453f3fffb924ca4ae3f8d34cabc03fbcae0127/org-roam-capture.el#L43), I have access to the canonical node creation time right in the file name.

```el
(defcustom org-roam-capture-templates
  '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n")
```

With this format in mind, I use a regular expression to match the datetime format of the file name.

```el
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
```

Once I have the datetime extracted into a string, I use two helper functions to convert it into an org-friendly date.

First, I used a modified version of `org-timestamp-from-time` that can parse the datetime string we created and convert it into an `org-timestamp`.

```el
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
```

I then format the `org-timestamp` _back_ into a string but formatted the same as the timestamp used when we create a new file.

```el
(defun tr/format-org-date (date)
  (org-timestamp-format date "\[%Y-%02m-%02d %3a %02H:%02M\]"))
```

To fetch the `last_modified` value, we can use the `file-attributes` function from `Emacs` itself. After we grab the modification time from our file attributes, we can pass it into the function we cribbed from above (`org-timestamp-from-time`) and reuse our formatting function.

```el
(defun tr/modification-timestamp (fpath)
  "Use file-attributes to get the modification time of a file and convert it to an
  org-timestamp"
  (tr/format-org-date
   (org-timestamp-from-time
    (file-attribute-modification-time (file-attributes fpath)))))
```

Knowing I want to use both values together, I wrote a function that calls both helpers and wraps them in a list.

```el
(defun tr/file-datetime-info (fpath)
  "Get a list containing a file's creation and change datetime"
  (list (tr/file-creation-time-from-name fpath)
        (tr/modification-timestamp fpath)))
```

### Adding timestamps to a file

Now that, for a given file, we can get our two timestamps, we can use our `tr/file-datetime-info` function to insert these values into our files. using the following function.

```el
(defun tr/add-time-stamp (fpath)
  "Add `created_at` and `last_modified` timemstamps keywords to file"
  (message (format "Checking file %s" fpath))
  (setq case-fold-search t)
  (find-file-other-window fpath)
  (goto-char (point-min))

  ;; don't add if we already have `created_at'
  (unless (search-forward "#+created_at" 'nil t)
    (progn
      (pcase-let ((`(,created-time ,modified-time) (tr/file-datetime-info fpath)))
        (goto-char (point-min))
        (search-forward "#+title")
        (end-of-line)
        (newline)
        (insert (format "#+created_at: %s\n#+last_modified: %s" created-time modified-time))
        (write-file fpath))))
  (other-window 1))
```

This function will:

1.  Open the given file in another window
2.  Go to the top of the file
3.  Check if the `create_at` field exists before moving on. Since I had created some new files using my new template and was debugging my code as I went, this allowed me to skip files I may have already updated.
4.  Insert the two datetimes after the `title` keyword, matching our new file template above

I don't know if it was necessary to open the file in another window, but, during development, it was helpful to have the file I was manipulating already open. Also, when running this in bulk, neither the `message` nor the other window provided much feedback because files were processed so quickly.

### Updating all the files

Now that we have all of the pieces in place to update a single file, updating all nodes is simple. We can leverage [`org-roam-directory`](https://github.com/org-roam/org-roam/blob/7f453f3fffb924ca4ae3f8d34cabc03fbcae0127/org-roam.el#L115-L119) to find all of our `org-roam-files` and call `tr/add-time-stamp` for each.

```el
 (let ((files (directory-files org-roam-directory 'full ".org")))
   (dolist (file files) (tr/add-time-stamp file)))
```

## Updating the timestamps

Our final step is keeping our `last_modified` value up-to-date. To do this, we can leverage a `before-save-hook`.

Based on [this post](https://org-roam.discourse.group/t/update-a-field-last-modified-at-save/321/18), the hook leverages the built-in [`time-stamp` module](https://www.emacswiki.org/emacs/TimeStamp) to find and update the timestamp after our `last_modified` keyword.

```el
(after! org
  (setq time-stamp-active t
        time-stamp-start "#\\+last_modified: [\t]*"
        time-stamp-end "$"
        time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
  (add-hook 'before-save-hook 'time-stamp))
```

Two things to note:

1.  The `after!` keyword is [defined in Doom](https://github.com/doomemacs/doomemacs/blob/c44bc81a05f3758ceaa28921dd9c830b9c571e61/lisp/doom-lib.el#L496). If you are not using Doom, you may need to adjust if you want this to only load for `org-mode` files.
2.  This hook will run in all of your org files. I have not noticed a performance concern when saving, but, if you do, you may want to investigate limiting the scope of the hook.

## Conclusion

While I don't yet know if the decision to add these timestamps will prove to provide value or simply satisfy a curiosity, this work may be an example of getting more from the journey than the destination. My journey has taken me a step deeper into the world of Emacs and the power it provides in enabling package configuration (updating my templates), editor behavior (adding save hooks), and bulk editing files.
