---
title: 'Contributing GitHub Actions'
date: '2020-10-09T05:47:13.265Z'
categories: ['open source', 'github actions']
---

[GitHub Actions](https://github.com/features/actions) are a feature of GitHub that allow you to have GitHub run some code for you in a container. Actions open up a wide array possiblities - from running tests to greeting new contributors.

This post is not about GitHub Actions themselves, and instead about how you can test out and add Actions to open source projects.

If you are unfamiliar with GitHub Actions, I would recommend checking out ther [marketing page](https://github.com/features/actions) which links to the documentation and marketplace for further exploration. 

## Why does this need a blog post

GitHub Actions are set up and configured with `yml` files. This means that you can create a new file that tells the Action what to do and make a Pull Request to add it to a repository. 

If you are leveraging existing Actions, the [marketplace](https://github.com/marketplace?type=actions) does a great job of making this easy as well by having an install-like button (currently labeled as "Use latest version") that will give you a piece of the YAML needed to reference the Action.

<img src='./use-latest-gh-action.png' lazy />

Many Actions will include more complete examples of the YAML file that can be directly copied and pasted:

<img src='./gh-action-readme-setup.png' lazy />

So, what's the problem here? If all we need is a text file, and we can get the content of that fils by copying and pasting, how is this not even easier than other open source contributions? 

## Running Actions

The biggest issue with contributing a GitHub action to another project is that Actions only seem to run when they committed to the repository's main branch. This means that if you open a Pull Request that adds a GitHub Action, it will **not be run** as a part of that Pull Request.

This limitation makes sense, especially with the power that Actions can provide. You don't want empower anyone on the internet to be able to open a Pull Request and run arbitrary code with an Action. This can be potentially dangerous when bad actors are involved.

Despite it making sense, it does make testing a bit difficult. While many actions are designed to be easy to run, there are still often configuration options that you may want to experiment with before comitting anything to your main branch. 

The insiration for this post came from wanting to add the [Pull Request Labeler Action](https://github.com/actions/labeler) to an open source project. This Action requires a configuration file written in YAML that lists different file path glob patterns and which label to add to the Pull Request if a matching file was chanaged. While the list of labels the project was hoping to have auto-added was relatively short, there were still a number of patterns that had to be added.

On my first attempt, I made the Pull Request and realized the Action wouldn't run. Not being aware of a better way to test, I said as much in the Pull Request body. The project's mantainers decide to try merging the Pull Request and manually testing the action by creating a fake Pull Request. Unfortunately, the Action was failing to run. The mantainers ended up needing to revert my change because it was causing the GitHub Checkes to fail and would block merging future Pull Request. 

I want to fix the Action, but wanted to avoid having to merge before testing could happen. Especially when it disrupts other work on the project. 

## It's Your Fork

For this work I was attempting to add an Action to a project I didn't own. This means I was working off of a [fork](https://docs.github.com/en/free-pro-team@latest/github/getting-started-with-github/fork-a-repo) of the project. Fortunately, in GitHub, when you fork a project you have control over the project like it's your own. Often, when forking open source project you are simply using it was a separate place to make code changes. You usually do not need GitHub functionality like Issue tracking, Wikis, or even Actions. Luckily, you can enable them if you do want them though. 

In this case, since I was attempting to contribute an Action, I wanted to enable to Action functionality for my fork. 

While, even on my fork, Actions would only run when they were merged into the main branch, merging into `master` wasn't as dangerous an activity on my fork as it would be on the actual project. If things went wrong I could reset my `master` branch to synchronize with the project's real `mater` branch.

This meant that I could add the Action directly to master and iterate on it (again, on master) until I got it right.

Also, since I was testing an Action that only worked when Pull Requests were created, I was able to create multiple Pull Requests without worrying about emails being sent to people maintaining or watching the project. 

Once I was happy with how the Action was working I created a new branch off of the main branch, collected screenshots of the Action in action, and opened up a Pull Request on the real project. This time, I was able to have some confidence that things were tested. 

### Additional Benefit

One additional benefit to using your own fork as a testing ground is the ability to use GitHub's UI for creating and editing an Action. When you are not a part of an Organization, you cannot leverage the GitHub's Actions UI. While, as mentioned before, the configuration for Actions is simply a `yml` file that you can manually create and edit, the UI provides some useful assitance. 

With the UI, initial Action creation is much easier because you can select existing actions to use and it will generate the basic `yml` that pulls in that Action.

When using the UI's editor, you will get syntax highlighting and some basic IntelliSense-like autocompletion to help guide you. This can help catch typos and formatting issues more quickly than having to commit changes to your main branch and wait for the Action to run. In addition to the IntelliSense as a form of documentation, there is also a side panel that includes high-level Documentation (with links to dig deeper). Since Actions are still relatively new and you probably aren't regularly creating new ones, having this refresher is nice. 

While not necessary, if you haven't worked with Actions much yet, I would suggest initalizing your Action through the GitHun UI when you can. Since we are already working off of our own fork, this makes the proecess of contributing an Action to another process even easier.

## Testing in Your Own Project

This tip works well when Contributing an Action to a project you don't own, because you will likely need to have a fork of the projectanyway. When working in a fork, you can bend some of the rules around working off of the main branch, because you can reset everything based on the upstream project's real code whenever you need to. 

This doesn't, however, work as well for projects you own or projects that you contribute to to directly (like you probably do at work). 

Unfortunately, at this time, I am not sure of a workflow that lets you iterate as freely on changes to an Action when working in this situation. If you have any suggestions, please reach out and let me know! I would love to update this post to include a suggestion for this situation.

## Conclusion




