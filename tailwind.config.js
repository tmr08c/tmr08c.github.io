module.exports = {
  theme: {
    // Some useful comment
    extend: {
      fontSize: {
        "8xl": "5rem"
      },
      colors: {
        // PAANTONE's 2019 Color of the Year
        // https://www.pantone.com/color-intelligence/color-of-the-year/color-of-the-year-2019
        "living-coral-500": "#FF6F61"
      }
    }
  },
  variants: {
    // Some useful comment
    textColor: ["responsive", "hover", "focus", "active", "group-hover"],
    borderStyle: ["responsive", "hover", "focus", "active", "group-hover"]
  },
  plugins: [
    // Some useful comment
  ],
  // Using the built-in support for purging CSS
  // https://tailwindcss.com/docs/controlling-file-size/
  purge: [
    './src/**/*.html',
    './src/**/*.tsx',
    './src/**/*.jsx',
  ],
  // opt-in to future breaking changes
  future: {
    // Will be the default in 2.0
    removeDeprecatedGapUtilities: true,
  },
};
