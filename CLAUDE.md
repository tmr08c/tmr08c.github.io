# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal blog built with Hugo (Go static site generator) using the hugo-texify3 theme. The blog focuses on technology topics including Ruby, Rails, Elixir, Phoenix, JavaScript, and engineering management.

## Essential Commands

### Development
```bash
./bin/setup            # Initial setup (install dependencies)
./bin/dev              # Start development server at localhost:1313 (includes drafts)
./bin/server           # Start production-like server at localhost:1313
./bin/build            # Build for production
```

### Testing
```bash
npm run test:e2e:ci    # Run E2E tests in CI mode
npm run test:e2e       # Run E2E tests with interactive Cypress UI
npm run cy:open        # Open Cypress test runner directly
```

### Content Creation
```bash
hugo new blog/YYYY/MM/post-title/index.md    # Create new blog post with archetype
hugo list drafts                              # List all draft content
hugo list future                              # List future-dated content
```

### Deployment

Site deploys automatically via GitHub Actions on push to `main`. The workflow builds with Hugo and deploys to GitHub Pages.

## Architecture

### Content Structure
- Blog posts are markdown files in `content/blog/YYYY/MM/post-name/index.md` with co-located assets
- Static assets go in `static/`
- Theme is a git submodule in `themes/hugo-texify3/`

### Key Technologies
- **Static site generator**: Hugo (Go-based)
- **Theme**: hugo-texify3 (LaTeX-style with Computer Modern fonts)
- **CSS**: Dart Sass + PostCSS
- **Testing**: Cypress E2E tests with start-server-and-test
- **Deployment**: GitHub Actions -> GitHub Pages
- **Content**: Markdown with YAML frontmatter

### Important Files
- `hugo.toml` - Main Hugo configuration (permalinks, menus, markup settings)
- `layouts/` - Template overrides
- `assets/` - Custom CSS/JS
- `postcss.config.js` - PostCSS configuration
- `.tool-versions` - asdf tool versions (Hugo, Node.js)

### Custom Shortcodes
- `video-simple` - Responsive video embed with configurable aspect ratio

### Custom Bin Scripts
- `./bin/dev` - Development server with drafts and future content enabled
- `./bin/server` - Production-like server without drafts
- `./bin/build` - Production build script
- `./bin/setup` - Initial setup script

### Git Submodule Management
The hugo-texify3 theme is managed as a git submodule. When cloning or updating:
```bash
git submodule update --init --recursive  # Initialize submodules
git submodule update --remote           # Update theme to latest
```

### Blog Post Structure
New posts should follow the established pattern:
- Location: `content/blog/YYYY/MM/descriptive-title/index.md`
- Co-locate images and assets in the same directory as `index.md`
- Use YAML frontmatter with title, date, and optional tags

## Testing

E2E tests use Cypress and are in `cypress/e2e/`. Tests run against Hugo server on port 8000 (not default 1313). CI runs tests in headless mode.
