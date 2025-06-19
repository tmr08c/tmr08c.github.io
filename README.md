# TroyProg Blog

The personal blog of [@tmr08c](https://github.com/tmr08c/).

## Repository Structure

This repository contains two versions of the blog as we transition from Gatsby to Hugo:

- `gatsby-site/` - The original Gatsby-based blog
- `hugo-site/` - The new Hugo-based blog (in development)

## Development

### Branching

This blog is leveraging GitHub pages. Because it follows the `<username>.github.io` repo name pattern, it expects `master` to contain the published version of the site.

From [the docs](https://help.github.com/en/articles/configuring-a-publishing-source-for-github-pages):

> If your site is a User or Organization Page that has a repository named
> <username>.github.io or <orgname>.github.io, you cannot publish your site's
> source files from different locations. User and Organization Pages that have
> this type of repository name are only published from the master branch.

As a result, work is done on `develop`, and `master` is the published version of the site.

### Gatsby Site

The Gatsby version is the current production site. To work with it:

```bash
cd gatsby-site
npm install
npm start  # Development server at localhost:8000
npm run build  # Build for production
npm run deploy  # Deploy to GitHub Pages
```

### Hugo Site

The Hugo version is under development. To work with it:

```bash
cd hugo-site
hugo server  # Development server at localhost:1313
hugo  # Build for production
```

## Deployment

Currently using Gatsby:

```bash
cd gatsby-site
npm run deploy
```

Once the Hugo migration is complete, deployment will switch to the Hugo version.

## Credits

- Gatsby site built with [Gatsby](https://www.gatsbyjs.org/) - the blazing-fast static site generator for [React](https://facebook.github.io/react/).
- Gatsby project started using the [gatsby-typescript-starter-blog](https://github.com/frnki/gatsby-typescript-starter-blog) starter project.
- Hugo site uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme.
