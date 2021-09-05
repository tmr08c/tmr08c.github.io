const colors = require("tailwindcss/colors");

module.exports = {
  darkMode: "class",
  theme: {
    colors: colors,
    extend: {
      animation: {
        wiggle: "wiggle 3s ease-in-out infinite",
      },
      fontSize: {
        "8xl": "5rem",
      },
      keyframes: {
        wiggle: {
          "0%, 100%": { transform: "rotate(0deg)" },
          "25%, 75%": { transform: "rotate(15deg)" },
          "50%": { transform: "rotate(-15deg)" },
        },
      },
      typography: (theme) => ({
        DEFAULT: {
          css: {
            // turn off @tailwindcss/typography default `pre`
            // styling preferring remark gatsby plugin
            pre: false,
            a: {
              "&:hover": {
                color: theme("colors.purple.400"),
              },
            },
          },
        },
        light: {
          css: [
            {
              color: theme("colors.gray.200"),
              '[class~="lead"]': {
                color: theme("colors.gray.300"),
              },
              a: {
                color: theme("colors.white"),
                "&:hover": {
                  color: theme("colors.purple.400"),
                },
              },
              strong: {
                color: theme("colors.white"),
              },
              "ol > li::before": {
                color: theme("colors.gray.400"),
              },
              "ul > li::before": {
                backgroundColor: theme("colors.gray.600"),
              },
              hr: {
                borderColor: theme("colors.gray.200"),
              },
              blockquote: {
                color: theme("colors.gray.200"),
                borderLeftColor: theme("colors.gray.600"),
              },
              h1: {
                color: theme("colors.white"),
              },
              h2: {
                color: theme("colors.white"),
              },
              h3: {
                color: theme("colors.white"),
              },
              h4: {
                color: theme("colors.white"),
              },
              "figure figcaption": {
                color: theme("colors.gray.400"),
              },
              code: {
                color: theme("colors.white"),
              },
              "a code": {
                color: theme("colors.white"),
              },
              pre: false,
              thead: {
                color: theme("colors.white"),
                borderBottomColor: theme("colors.gray.400"),
              },
              "tbody tr": {
                borderBottomColor: theme("colors.gray.600"),
              },
            },
          ],
        },
      }),
    },
  },
  variants: {
    textColor: [
      "dark",
      "responsive",
      "hover",
      "focus",
      "active",
      "group-hover",
    ],
    borderWidth: ["dark"],
    borderStyle: [
      "dark",
      "responsive",
      "hover",
      "focus",
      "active",
      "group-hover",
    ],
    animation: ["responsive", "hover", "motion-safe", "motion-reduce"],
    typography: ["dark"],
  },
  plugins: [require("@tailwindcss/typography")],
  // Using the built-in support for purging CSS
  // https://tailwindcss.com/docs/controlling-file-size/
  purge: ["./src/**/*.html", "./src/**/*.tsx", "./src/**/*.jsx"],
};
