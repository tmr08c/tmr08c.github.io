module.exports = {
  theme: {
    // Some useful comment
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
      }),
    },
  },
  variants: {
    textColor: ["responsive", "hover", "focus", "active", "group-hover"],
    borderStyle: ["responsive", "hover", "focus", "active", "group-hover"],
    animation: ["responsive", "hover", "motion-safe", "motion-reduce"],
  },
  plugins: [require("@tailwindcss/typography")],
  // Using the built-in support for purging CSS
  // https://tailwindcss.com/docs/controlling-file-size/
  purge: ["./src/**/*.html", "./src/**/*.tsx", "./src/**/*.jsx"],
};
