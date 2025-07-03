# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal blog built with Hugo (Go static site generator) using the hugo-texify3 theme. The blog focuses on technology topics including Ruby, Rails, Elixir, Phoenix, JavaScript, and engineering management.

## Essential Commands

### Development
```bash
cd hugo-site                    # Navigate to Hugo site directory
./bin/dev                      # Start development server with drafts at localhost:1313
./bin/server                   # Start production-like server at localhost:1313
hugo server                    # Basic Hugo server
hugo                          # Build production site to public/ directory
```

### Testing
```bash
cd hugo-site                   # Navigate to Hugo site directory
npm install                   # Install test dependencies (first time only)
npm run test:e2e              # Run E2E tests with interactive Cypress UI
npm run test:e2e:ci           # Run E2E tests in headless CI mode
npm run cy:open               # Open Cypress test runner directly
```

### Content Creation
```bash
cd hugo-site
hugo new blog/YYYY/MM/post-title/index.md    # Create new blog post with archetype
hugo list drafts                              # List all draft content
hugo list future                              # List future-dated content
```

### Deployment
```bash
# Automatic deployment via GitHub Actions when pushing to main branch
# Manual deployment can be triggered from GitHub Actions tab
# Site deploys to: https://tmr08c.github.io/
```

## Architecture

### Content Organization
- **Blog posts**: `hugo-site/content/blog/YYYY/MM/post-name/index.md` with co-located assets
- **Static files**: `hugo-site/static/` for site-wide assets (favicon, etc.)
- **Theme assets**: `hugo-site/themes/hugo-texify3/` (git submodule from GitHub)
- **Custom layouts**: `hugo-site/layouts/` for overriding theme templates

### Hugo Configuration Highlights
- **Permalinks**: Posts use `/:year/:month/:slug/` format
- **Outputs**: Generates both HTML and RSS feeds
- **Markup**: Goldmark renderer with syntax highlighting enabled
- **Navigation**: Main menu includes Blog, RSS, and GitHub links
- **Theme features**: Table of contents, social sharing, custom CSS/JS support

### Development Workflow
- Use `./bin/dev` for development (includes drafts and future posts)
- Content is primarily markdown with YAML frontmatter
- Hugo rebuilds automatically on file changes during development
- Cypress tests verify site functionality after builds

### Key Technologies
- **Static Site Generator**: Hugo (Go-based)
- **Theme**: hugo-texify3 (git submodule)
- **Styling**: PostCSS pipeline + theme SCSS
- **Testing**: Cypress E2E tests with start-server-and-test
- **Deployment**: GitHub Actions → GitHub Pages
- **Content**: Markdown with YAML frontmatter

## Important Implementation Details

### Git Submodule Management
The hugo-texify3 theme is managed as a git submodule. When cloning or updating:
```bash
git submodule update --init --recursive  # Initialize submodules
git submodule update --remote           # Update theme to latest
```

### Blog Post Structure
New posts should follow the established pattern:
- Location: `hugo-site/content/blog/YYYY/MM/descriptive-title/index.md`
- Co-locate images and assets in the same directory as `index.md`
- Use YAML frontmatter with title, date, and optional tags

### Custom Bin Scripts
The `hugo-site/bin/` directory contains convenience scripts:
- `./bin/dev` - Development server with drafts and future content enabled
- `./bin/server` - Production-like server without drafts
- `./bin/build` - Production build script
- `./bin/setup` - Initial setup script

### Testing Strategy
- E2E tests in `hugo-site/cypress/e2e/` verify core site functionality
- Tests run against Hugo server on port 8000 (not default 1313)
- CI runs tests in headless mode, development uses interactive mode

## Deployment Pipeline

Site deploys automatically via GitHub Actions:
1. **Trigger**: Push to `main` branch or manual workflow dispatch
2. **Build**: Hugo generates static site with production optimizations
3. **Deploy**: GitHub Pages publishes to https://tmr08c.github.io/
4. **Dependencies**: Uses Hugo version specified in `hugo-site/.tool-versions`