# TroyProg Hugo Site

This is a Hugo version of the TroyProg blog, migrated from Gatsby. It uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme for a LaTeX-style appearance.

## Quick Start

1. **Initial setup** (run once):
   ```bash
   ./bin/setup
   ```

2. **Start development server**:
   ```bash
   npm run dev
   # or
   hugo server --bind 0.0.0.0 --port 1313 --buildDrafts
   ```

3. **Build for production**:
   ```bash
   npm run build
   # or
   hugo --minify
   ```

## Dependencies

This project requires several tools that are managed automatically by the setup script:

- **Hugo** - Static site generator
- **Dart Sass** - CSS preprocessor for the theme
- **Node.js & npm** - For PostCSS and other build tools
- **PostCSS** - CSS post-processing

### Optional but Recommended

- **asdf** - Version manager for consistent tool versions across environments

## Project Structure

```
hugo-site/
├── content/posts/       # Blog posts (migrated from Gatsby)
├── themes/hugo-texify3/ # Theme submodule
├── static/              # Static assets
├── hugo.toml           # Hugo configuration
├── package.json        # Node.js dependencies
├── postcss.config.js   # PostCSS configuration
├── Brewfile            # Homebrew dependencies
├── .tool-versions      # asdf tool versions
└── bin/setup           # Setup script
```

## Migration Notes

- All 63 blog posts have been migrated from the original Gatsby site
- Frontmatter has been converted from Gatsby to Hugo format
- Images and assets have been preserved with the same relative paths
- Raw HTML rendering is enabled in the Hugo configuration

## Development

The site is configured to run on `localhost:1313` during development. The `--buildDrafts` flag is enabled by default in the development script to show all content.

## Theme

This site uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme, which provides:

- LaTeX-style typography with Computer Modern fonts
- Dark mode toggle
- Gruvbox color scheme
- Math rendering with KaTeX
- Responsive design
- SEO optimization