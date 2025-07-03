# TroyProg Blog

The personal blog of [@tmr08c](https://github.com/tmr08c/).

## Repository Structure

This repository contains the Hugo-based blog in the `hugo-site/` directory.

## Development

### Branching

This blog is leveraging GitHub Pages. Work is done on the `main` branch, and GitHub Actions automatically deploys to GitHub Pages when changes are pushed to `main`.

### Hugo Site

To work with the Hugo site:

```bash
cd hugo-site
hugo server  # Development server at localhost:1313
hugo  # Build for production
```

### Testing

To run E2E tests:

```bash
cd hugo-site
npm install  # Install test dependencies
npm run test:e2e  # Run tests with interactive UI
npm run test:e2e:ci  # Run tests in CI mode
```

## Deployment

Deployment is handled automatically by GitHub Actions when changes are pushed to the `main` branch. The site is built using Hugo and deployed to GitHub Pages.

## Credits

- Built with [Hugo](https://gohugo.io/) - the world's fastest framework for building websites.
- Uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme.
