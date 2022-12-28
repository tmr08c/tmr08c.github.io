---
title: "Creating Org-roam node from an elfeed entry"
date: "2022-12-29T11:15:02.123Z"
categories: ["emacs", "org-mode", "org-roam", "elfeed"]
---

I had a very Emacs-y day while recently on PTO. It started with using [elfeed](https://github.com/skeeto/elfeed) to read articles in my RSS feed. After coming across an article I wanted to take some notes on, I stayed within Emacs to use my note taking tool of choice, [Org-roam](https://www.orgroam.com/). This is a regular occurrence for me and a process I do &ldquo;by hand.&rdquo; Knowing I had an open afternoon, I embarked on the most Emacs-y part of the day—attempting to find a way automate part of this process.

## My current flow

My current process for creating a new `node` from an `elfeed` entry isn&rsquo;t particular cumbersome:

1.  I will use the shortcut to create a new Org-roam `node`.
2.  I add a `filetags` property to my node and add the tag, `article`. I am not sure this part of my workflow will stay, but I have experimenting with tags such as `video` and `article` for content I have consumed, thinking may make it easier to find in the future.
3.  I will set the title to the title used in the entry. I don&rsquo;t have an elegant way to do this, and end up copying the full entry line from the `elfeed-search` buffer. This capture the title, author, and tags I may have added. I will just paste this and delete everything but the title.
4.  I store a link to the `elfeed` entry using `org-store-link` (some magic from DOOM or `elfeed` or something else makes this just work&trade;).
5.  I paste the `elfeed` link into capture pane for the `node`.
6.  I write some notes.

<center>
    <video controls>
        <source src="./elfeed-flow-before.mp4" type="video/mp4" />
    </video>
</center>

This requires some diligence (using the right title, remembering to include the link), window jumping, and text clean-up, but isn&rsquo;t a time-consuming process. However, because I had some holiday time-off, I decided to use this as an opportunity to explore building my skills with customizing Emacs to my liking and explore writing a function to more quickly create and pre-populate an Org-roam `node` based on the current `elfeed` entry I am viewing.

## My target flow

Inspired by my workflow above, I wanted to a function that would:

1.  Create a new Org-roam `node`.
2.  Set the title to match the title of the article I am reading.
3.  Add the `article` filetag.
4.  Include a link to the `elfeed` entry in the body of the note

Ultimately, looking something like:

```org-mode
,:PROPERTIES: ;; managed by Org-roam
#+TITLE: Title of Article
#+ filetags: "article"

link :: elfeed:link-to-article
```

Lucky for me, [others have implemented this same flow](https://takeonrules.com/2022/02/07/org-roam-emacs-and-ever-refining-the-note-taking-process/). The focus of that blog post is a more intricate `Org-roam` workflow, involving creating notes within &ldquo;context&rdquo; and auto-labeling work. While not central to the post, it does include references to how they capture notes on `elfeed` articles. This article is definitely worth a read, but since the author&rsquo;s workflow is a bit more advanced, I thought it would still be worth writing a post more directly related to creating a new `Org-roam` node based on an `elfeed` entry.

## Creating a template

Since we know what we want our template to look like, let&rsquo;s start there. I&rsquo;ve [updated my Org-roam capture templates before](/2022/08/add-timestamps-to-org-files/#adding-the-timestamps) and this will be a similar process, `Org-roam` ships with [a single capture template](https://github.com/org-roam/org-roam/blob/c3867619147175faf89ed8f3e90a1e67a4fd9655/org-roam-capture.el#L41-L45), set via `org-roam-capture-templates`. The default template simply sets the title property, but is otherwise empty. Our ideal template expands upon this by adding (1) the file tags property and (2) the link to our article. We will accommodate including this new data by adding a new capture template to our list.

```emacs-lisp
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

To remain consistent across our capture templates, we define the variables `file-name` and `properties`. With only two templates these variables may look like overkill. In my actual config I have [expanded the default properties I wanted to include in all of my capture templates](/2022/08/add-timestamps-to-org-files/#adding-the-timestamps) and started experimenting with a [refs slipbox](https://jethrokuan.github.io/org-roam-guide/), so these variables have made my configuration easier to read and keep consistent. You can see my full, current `org-roam-capture-template` setting here (TODO-merge branch and get link).

Our new entry, named `elfeed` is set to the key `e` (we will need to know this later). It uses the same file naming-scheme as the default template, but expands upon the properties by adding in our `filetags` setting. We also set some text to be used in the body, `link :: ${link}`. Here, we leverage an additional [substitution syntax supported by `Org-roam`](https://github.com/org-roam/org-roam/blob/c3867619147175faf89ed8f3e90a1e67a4fd9655/org-roam-capture.el#L271-L275).

> In addition to all of the above, Org-roam supports additional
> substitutions within its templates. &ldquo;${foo}&rdquo; will look for the
> foo property in the Org-roam node (see the org-roam-node). If
> the property does not exist, the user will be prompted to fill in
> the string value.

If we manually invoke this capture template, we will be prompted to provide a `link` at capture-time. However, we will be writing a function to create our node and will be able to pass in the `link` as a property on our node. Let&rsquo;s see how to do this now.

## A function to create our node

Now that we have a capture template for our `elfeed`-based `Org-roam` entry, we can write a function to create a node based on that template, populating it with data from an `elfeed` entry. Here&rsquo;s what we will end up with:

```emacs-lisp
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

We start by fetching our current `elfeed` entry with `elfeed-show-entry` and extracting the `title` using `elfeed`&rsquo;s helper, `elfeed-entry-title`. We use another `elfeed` helper function, `elfeed-link-store-link` to get access to the link.

These variables are fed into our call to `org-roam-capture-` (note the trailing dash) to create a new entry. Our entry uses the template we defined above by setting by the `:keys` attribute to match the `key` we associated the template with (&ldquo;e&rdquo;). We pass in the `title` as a part of the `node` and the `link` via the [`INFO` property](https://github.com/org-roam/org-roam/blob/d95d25615e69e7cc847641800c1886366336c97e/org-roam-capture.el#L401) which &ldquo;is a plist for filling up `Org-roam`&rsquo;s capture templates.&rdquo; By setting `:link` in our plist, we provide the substitution necessary for the `${link}` in the template we created above.

## Putting it together

And that&rsquo;s it! With our new template and function, we now have everything we need to quickly create an `Org-roam` node based on the `elfeed` we are viewing. Let&rsquo;s see it in action.

<center>
    <video controls class="center">
        <source src="./elfeed-flow-after.mp4" type="video/mp4" />
    </video>
</center>
