# TroyProg Hugo Site

This is a Hugo version of the TroyProg blog, migrated from Gatsby. It uses the [hugo-texify3](https://github.com/michaelneuper/hugo-texify3) theme for a LaTeX-style appearance.

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

The site runs on `localhost:1313` during development. The `./bin/dev` script includes drafts and future-dated content so you can see everything while developing.

The development server binds to `0.0.0.0` to allow access from other devices on your network.

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

**Problem it solves**: The default HTML `<video>` tag wrapped in `<center>` was causing horizontal scroll issues on mobile devices and not maintaining proper aspect ratios.

**Features**:
- Maintains aspect ratio (16:9 by default, configurable)
- Fully responsive on all screen sizes
- Prevents horizontal scroll overflow
- Lazy loading support with `preload="none"`
- Black background while video loads
- Maximum width constraint (800px) on larger screens

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