---
date: '2022-12-29T11:15:02'
slug: org-roam-node-from-elfeed-entry
tags:
- emacs
- org-mode
- org-roam
- elfeed
title: Creating Org-roam node from an elfeed entry
---

I had a very Emacs-y day while on PTO recently.

It began with starting up [`elfeed`](https://github.com/skeeto/elfeed) to read articles in my RSS feed. After coming across an article I wanted to take notes on, I turned to my note-taking tool of choice, [`Org-roam`](https://www.orgroam.com/). This RSS to note-taking workflow is a regular occurrence for me and a process I do "by hand." Knowing I had an open afternoon, I embarked on the most Emacs-y part of the day: attempting to find a way to automate part of this process.

This post covers how I wrote a function to create a new `Org-roam` note based on the `elfeed` entry I am currently viewing.

## My current flow

My current process for creating a new `node` from an `elfeed` entry isn't particularly cumbersome:

1. I use the shortcut to create a new Org-roam `node`.
2. I add a `filetags` property to my node and add the tag `article` to the list. I am not sure this part of my workflow will stay, but I have been experimenting with tags such as `video` and `article` for content I have consumed, thinking they may make it easier to find the note in the future (for example, I may recall watching a video about a topic, but not the full talk title).
3. I set the `node`'s title to the title associated with the entry. I don't have an elegant way to do this, so I copy the full entry line from the `elfeed-search` buffer, capturing the title, author, and tags. When creating the entry, I paste the full line, deleting everything but the title.
4. I store a link to the `elfeed` entry using `org-store-link` (some magic from DOOM or `elfeed` or something else makes this just work&trade;).
5. I paste the `elfeed` link into the capture pane for the `node`.
6. I write up any notes I want to take.

{{< video-simple src="./elfeed-flow-before.mp4" type="video/mp4" controls="true" ratio="16:9" preload="none" >}}

This manual process requires diligence (using the correct title, remembering to include the link), window jumping, and text clean-up, but, as the video above shows, is not overly time-consuming. However, because I had some time off, I wanted to see if I could speed up the process, improving my Emacs skills along the way.

## My target flow

Inspired by my workflow above, I wanted a function that would:

1. Create a new `Org-roam` `node`
2. Set the title to match the title of the article I am reading
3. Add the `article` filetag
4. Include a link to the `elfeed` entry in the body of the note

Ultimately, I want a capture buffer, pre-filled to look something like the following.

```org-mode
:PROPERTIES: ;; managed by Org-roam
#+TITLE: Title of Article
#+filetags: :article:

link :: elfeed:link-to-article
```

Lucky for me, [others have implemented this same flow](https://takeonrules.com/2022/02/07/org-roam-emacs-and-ever-refining-the-note-taking-process/). The focus of that blog post is a more intricate `Org-roam` workflow, involving creating notes within "contexts" and helping pre-populate tags. While not central to the post, it does include references to how they capture notes on `elfeed` articles. Because the author's workflow is a bit more advanced, I thought it would be worth writing a post more directly related to creating a new `Org-roam` node based on an `elfeed` entry.

## Creating a template

Since we know what we want our template to look like, let's start there. I've updated my Org-roam capture templates [before](/2022/08/add-timestamps-to-org-files/#adding-the-timestamps), and this will be a similar process.

`Org-roam` ships with [a single capture template](https://github.com/org-roam/org-roam/blob/c3867619147175faf89ed8f3e90a1e67a4fd9655/org-roam-capture.el#L41-L45), set via `org-roam-capture-templates`. The default template simply sets the title property and is otherwise empty. Our ideal template expands upon this by adding (1) the file tags property and (2) the link to our article.

```el
(let* ((file-name "%<%Y%m%d%H%M%S>-${slug}.org")
       (properties "#+title: ${title}\n"))
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?" :target
           (file+head ,file-name ,properties)
           :unnarrowed t)
          ("e" "elfeed" plain "%?" :target
           (file+head ,file-name ,(concat properties "#+filetags: :article:\n\nlink :: ${link}\n\n"))
           :unnarrowed t))))
```

To remain consistent across our capture templates, we define the variables `file-name` and `properties`. With only two templates, these variables may be overkill. If you find you want to [add new, default properties to your capture templates](/2022/08/add-timestamps-to-org-files/) or use [multiple slipboxes](https://jethrokuan.github.io/org-roam-guide/), variables like these should make your configuration easier to read and keep consistent.

Our new entry, "elfeed," is set to the key `"e"` (we will reference this later). It uses the same file naming scheme as the default template but expands upon the properties by adding `filetags`. We also set some text to be used in the body, `link :: ${link}`, leveraging a [substitution syntax supported by `Org-roam`](https://github.com/org-roam/org-roam/blob/c3867619147175faf89ed8f3e90a1e67a4fd9655/org-roam-capture.el#L271-L275).

> '\${foo}' will look for the foo property in the Org-roam
> node (see the org-roam-node). If the property does not exist, the user will be
> prompted to fill in the string value.

If we manually invoke this capture template, we will be prompted to provide a `link` at capture time. However, when creating a template from a function, this substition enables us to _pass in_ the `link` for use at creation time. Let's see how to do this now.

## A function to create our node

Now that we have a capture template for our `elfeed`-based `Org-roam` entry, we can write a function to create a node based on that template, populating it with data from an `elfeed` entry. Here's what we will end up with:

```el
(defun tr/elfeed--create-roam-node ()
    "Create roam node from current elfeed entry"
    (interactive)
    (let* ((title (elfeed-entry-title elfeed-show-entry))
            (link (plist-get (elfeed-link-store-link) :link)))
    (org-roam-capture-
        :keys "e"
        :node (org-roam-node-create :title title )
        :info (list :link link))))
```

We start by fetching our current `elfeed` entry with `elfeed-show-entry` and extracting the `title` using `elfeed`'s helper, `elfeed-entry-title`. We use another `elfeed` helper function, `elfeed-link-store-link`, to get access to the link.

We then pass these variables into our call to `org-roam-capture-` (note the trailing dash) to create a new entry. Our entry uses the template we defined above by setting the `:keys` attribute to match the `key` we associated the template with (`"e"`). We pass in the `title` as a part of the `node` and the `link` via the [`INFO` property](https://github.com/org-roam/org-roam/blob/d95d25615e69e7cc847641800c1886366336c97e/org-roam-capture.el#L401) which "is a plist for filling up `Org-roam`'s capture templates." By setting `:link` in our plist, we provide the substitution necessary for the `${link}` in the template we created above.

## Putting it together

And that's it! With our new template and function, we now have everything we need to quickly create an `Org-roam` node based on the `elfeed` we are viewing. Let's see it in action:

{{< video-simple src="./elfeed-flow-after.mp4" type="video/mp4" controls="true" ratio="16:9" preload="none" >}}
