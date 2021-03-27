module.exports = {
  theme: {
    // Some useful comment
    extend: {
      fontSize: {
        "8xl": "5rem",
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
      }),
    },
  },
  variants: {
    textColor: ["responsive", "hover", "focus", "active", "group-hover"],
    borderStyle: ["responsive", "hover", "focus", "active", "group-hover"],
  },
  plugins: [require("@tailwindcss/typography")],
  // Using the built-in support for purging CSS
  // https://tailwindcss.com/docs/controlling-file-size/
  purge: ["./src/**/*.html", "./src/**/*.tsx", "./src/**/*.jsx"],
};
