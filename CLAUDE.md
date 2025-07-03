# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal blog built with Hugo (Go static site generator) using the hugo-texify3 theme. The blog focuses on technology topics including Ruby, Rails, Elixir, Phoenix, JavaScript, and engineering management.

## Essential Commands

### Development
```bash
cd hugo-site           # Navigate to Hugo site directory
hugo server            # Start development server at localhost:1313
hugo                   # Build production site
```

### Code Quality
```bash
# Content is primarily markdown files
```

### Testing
```bash
cd hugo-site           # Navigate to Hugo site directory
npm run cy:open        # Open Cypress test runner
npm run test:e2e       # Run E2E tests with dev server
npm run test:e2e:ci    # Run E2E tests in CI mode
```

### Deployment
```bash
# Automatic deployment via GitHub Actions when pushing to main branch
# Manual deployment can be triggered from GitHub Actions tab
```

## Architecture

### Content Structure
- Blog posts are markdown files in `hugo-site/content/blog/` organized by year/month
- Static assets go in `hugo-site/static/`
- Hugo configuration in `hugo-site/hugo.toml`
- Theme files in `hugo-site/themes/hugo-texify3/`

### Key Technologies
- **Static Site Generator**: Hugo (Go-based)
- **Theme**: hugo-texify3 (git submodule)
- **Styling**: PostCSS with theme-provided CSS
- **Code highlighting**: Hugo's built-in syntax highlighting
- **Testing**: Cypress for E2E tests
- **Deployment**: GitHub Actions to GitHub Pages

### Important Files
- `hugo-site/hugo.toml` - Main Hugo configuration
- `hugo-site/layouts/` - Custom layout templates (if any)
- `hugo-site/content/` - All content including blog posts
- `hugo-site/static/` - Static assets (images, files)
- `hugo-site/themes/hugo-texify3/` - Theme files (git submodule)

## Branching Strategy

- **main** - Main development and deployment branch
- **master** - Legacy branch (GitHub Pages publishes from main now)
- Work on `main`, GitHub Actions automatically deploys to GitHub Pages

## Testing

E2E tests are in `hugo-site/cypress/e2e/` and cover:
- Homepage navigation and content
- Blog post listing and navigation
- Site functionality

## Deployment

Site deploys to GitHub Pages via GitHub Actions. The deployment process:
1. Builds the Hugo site when code is pushed to `main`
2. Automatically deploys to GitHub Pages
3. Can also be triggered manually from the GitHub Actions tab