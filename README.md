# TroyProg Blog

The personal blog of [@tmr08c](https://github.com/tmr08c/).


## Development

### Branching

This blog is leveraging GitHub pages. Because it follows the `<username>.github.io` repo name pattern, it expects `master` to contain the published version of the site.

From [the docs](https://help.github.com/en/articles/configuring-a-publishing-source-for-github-pages):

> If your site is a User or Organization Page that has a repository named
> <username>.github.io or <orgname>.github.io, you cannot publish your site's
> source files from different locations. User and Organization Pages that have
> this type of repository name are only published from the master branch.

As a result, work is done on `develop`, and `master` is the published version of the site.

### Running the Server

```bash
# develop
npm start

# build
npm run build
```

## Deployment

```bash
npm run deploy
```

## Credits

Built with [Gatsby](https://www.gatsbyjs.org/) - the blazing-fast static site generator for [React](https://facebook.github.io/react/).

Project started using the [gatsby-typescript-starter-blog](https://github.com/frnki/gatsby-typescript-starter-blog) starter project.
