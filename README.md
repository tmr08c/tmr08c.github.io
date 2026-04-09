# TroyProg Blog

The personal blog of [@tmr08c](https://github.com/tmr08c/). Built with [Hugo](https://gohugo.io/) and the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme.

## Quick Start

1. **Initial setup** (run once):
   ```bash
   ./bin/setup
   ```

2. **Start development server**:
   ```bash
   ./bin/dev         # Includes drafts and future content
   ```

3. **Build for production**:
   ```bash
   ./bin/build
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
├── content/blog/        # Blog posts with co-located assets
├── themes/hugo-texify3/ # Theme submodule
├── static/              # Static assets
├── layouts/             # Template overrides
├── assets/              # Custom CSS/JS
├── hugo.toml           # Hugo configuration
├── package.json        # Node.js dependencies
├── postcss.config.js   # PostCSS configuration
├── Brewfile            # Homebrew dependencies
├── .tool-versions      # asdf tool versions
└── bin/                # Setup and dev scripts
```

## Development

The site runs on `localhost:1313` during development. The `./bin/dev` script includes drafts and future-dated content so you can see everything while developing.

The development server binds to `0.0.0.0` to allow access from other devices on your network.

### Testing

```bash
npm install              # Install test dependencies (first time only)
npm run test:e2e         # Run tests with interactive UI
npm run test:e2e:ci      # Run tests in CI mode
```

## Deployment

Deployment is handled automatically by GitHub Actions when changes are pushed to the `main` branch. The site is built using Hugo and deployed to GitHub Pages.

## Theme

This site uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme, which provides:

- LaTeX-style typography with Computer Modern fonts
- Dark mode toggle
- Gruvbox color scheme
- Math rendering with KaTeX
- Responsive design
- SEO optimization

## Custom Shortcodes

### video-simple

A responsive video shortcode that maintains proper aspect ratios and prevents layout breaking on mobile devices.

**Usage**:
```markdown
{{< video-simple src="./my-video.mp4" >}}

{{< video-simple 
    src="./demo-video.mp4" 
    type="video/mp4" 
    poster="./video-thumbnail.jpg"
    controls="true" 
    ratio="16:9" 
    preload="none" 
>}}
```

**Parameters**:
- `src` (required): Path to the video file
- `type` (optional): Video MIME type (default: "video/mp4")
- `poster` (optional): Path to poster image
- `controls` (optional): Show video controls (default: "true")
- `preload` (optional): Preload behavior (default: "none")
- `ratio` (optional): Aspect ratio - "16:9", "4:3", "21:9", or "1:1" (default: "16:9")

## Credits

- Built with [Hugo](https://gohugo.io/) - the world's fastest framework for building websites.
- Uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme.
