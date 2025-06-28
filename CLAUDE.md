# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal blog built with Gatsby (React static site generator) and TypeScript. The blog focuses on technology topics including Ruby, Rails, Elixir, Phoenix, JavaScript, and engineering management.

## Essential Commands

### Development
```bash
npm start              # Start development server at localhost:8000
npm run build          # Build production site
npm run prod           # Preview production build locally
```

### Code Quality
```bash
npm run format:write   # Format code with Prettier
npm run format:check   # Check formatting without changes
```

### Testing
```bash
npm run cy:open        # Open Cypress test runner
npm run test:e2e       # Run E2E tests with dev server
npm run test:e2e:ci    # Run E2E tests in CI mode
```

### Deployment
```bash
npm run deploy         # Build and deploy to GitHub Pages
```

## Architecture

### Content Structure
- Blog posts are markdown files in `content/blog/` organized by year/month
- Static assets go in `content/assets/`
- JSON data files (like talks) go in `content/data/`
- Static files (favicon, resume, robots.txt) go in `static/`

### Key Technologies
- **Styling**: TailwindCSS with dark mode support
- **Code highlighting**: gatsby-remark-vscode with VSCode themes
- **Images**: gatsby-image with Sharp for optimization
- **SEO**: React Helmet for meta tags
- **PWA**: Manifest and offline support configured

### Important Files
- `gatsby-config.js` - Main Gatsby configuration and plugins
- `gatsby-node.js` - Defines page creation from markdown files
- `src/templates/blog-post.tsx` - Blog post template
- `tailwind.config.js` - TailwindCSS configuration

## Branching Strategy

- **develop** - Main development branch
- **master** - Published version (GitHub Pages requirement for user.github.io repos)
- Work on `develop`, deploy pushes to `master`

## Testing

E2E tests are in `cypress/e2e/` and cover:
- Homepage navigation and content
- Blog post listing and navigation
- Dark mode functionality

## Deployment

Site deploys to GitHub Pages via the `gh-pages` package. The deployment process builds the site and pushes the `public/` directory to the `master` branch.