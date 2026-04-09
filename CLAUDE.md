# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal blog built with Hugo (static site generator) using the hugo-texify3 theme. The blog focuses on technology topics including Ruby, Rails, Elixir, Phoenix, JavaScript, and engineering management.

## Essential Commands

### Development
```bash
./bin/setup            # Initial setup (install dependencies)
./bin/dev              # Start development server at localhost:1313 (includes drafts)
./bin/build            # Build for production
```

### Testing
```bash
npm run test:e2e:ci    # Run E2E tests in CI mode
```

### Deployment

Site deploys automatically via GitHub Actions on push to `main`. The workflow builds with Hugo and deploys to GitHub Pages.

## Architecture

### Content Structure
- Blog posts are markdown files in `content/posts/` organized by year/month
- Static assets go in `static/`
- Theme is a git submodule in `themes/hugo-texify3/`

### Key Technologies
- **Static site generator**: Hugo
- **Theme**: hugo-texify3 (LaTeX-style with Computer Modern fonts)
- **CSS**: Dart Sass + PostCSS
- **Dark mode**: Built-in theme toggle
- **Math rendering**: KaTeX

### Important Files
- `hugo.toml` - Main Hugo configuration
- `layouts/` - Template overrides
- `assets/` - Custom CSS/JS
- `postcss.config.js` - PostCSS configuration
- `.tool-versions` - asdf tool versions (Hugo, Node.js)

### Custom Shortcodes
- `video-simple` - Responsive video embed with configurable aspect ratio

## Testing

E2E tests use Cypress and are in `cypress/e2e/`.
