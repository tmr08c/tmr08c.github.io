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
      },
      typography: _theme => ({
        DEFAULT: {
          css: {
            // turn off @tailwindcss/typography default `pre`
            // styling preferring remark gatsby plugin
            pre: false
          }
        }
      })
    }
  },
  variants: {
    textColor: ["responsive", "hover", "focus", "active", "group-hover"],
    borderStyle: ["responsive", "hover", "focus", "active", "group-hover"]
  },
  plugins: [require("@tailwindcss/typography")],
  // Using the built-in support for purging CSS
  // https://tailwindcss.com/docs/controlling-file-size/
  purge: ["./src/**/*.html", "./src/**/*.tsx", "./src/**/*.jsx"]
};
