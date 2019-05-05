module.exports = {
  siteMetadata: {
    title: "TroyProg",
    description:
      "This is the personal site for Troy Rosenberg - engineering manager and develop. This serves as a place to store blog posts that are generally about technology. Topics may include - Ruby, Ruby on Rails, Elixir, Phoenix, JavaScript, TypeScript, Node, project management, engineering mangement, and more!",
    author: "Troy Rosenberg (@tmr08c)"
  },
  plugins: [
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/blog`,
        name: "blog"
      }
    },
    {
      resolve: "gatsby-source-filesystem",
      options: {
        path: `${__dirname}/content/assets`,
        name: "assets"
      }
    },
    {
      resolve: `gatsby-transformer-remark`,
      options: {
        plugins: [
          {
            resolve: `gatsby-remark-images`,
            options: {
              maxWidth: 500
            }
          },
          {
            resolve: "gatsby-remark-external-links",
            options: {
              target: "_blank",
              rel: "nofollow noopener noreferrer"
            }
          },
          `gatsby-remark-responsive-iframe`,
          {
            resolve: `gatsby-remark-prismjs`
          },
          `gatsby-remark-copy-linked-files`
        ]
      }
    },
    "gatsby-plugin-typescript",
    "gatsby-plugin-react-helmet",
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: `images`,
        path: `${__dirname}/src/images`
      }
    },
    "gatsby-transformer-sharp",
    "gatsby-plugin-sharp",
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "troy-prog-blog",
        short_name: "troyprog",
        start_url: "/"
      }
    },
    "gatsby-plugin-offline",
    `gatsby-plugin-postcss`,
    {
      resolve: `gatsby-plugin-purgecss`,
      options: {
        printRejected: true, // Print removed selectors and processed file names
        tailwind: true
      }
    },
    {
      resolve: `gatsby-plugin-prefetch-google-fonts`,
      options: {
        fonts: [
          {
            family: `Chewy`
          }
        ]
      }
    },
    `gatsby-plugin-catch-links`,
    `gatsby-transformer-json`,
    {
      resolve: "gatsby-source-filesystem",
      options: {
        path: `${__dirname}/content/data`
      }
    }
  ]
};
