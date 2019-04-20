import { graphql, StaticQuery } from 'gatsby'
import Img from 'gatsby-image'
import * as React from 'react'

/*
 * This component is built using `gatsby-image` to automatically serve optimized
 * images with lazy loading and reduced file sizes. The image is loaded using a
 * `StaticQuery`, which allows us to load the image from directly within this
 * component, rather than having to pass the image data down from pages.
 *
 * For more information, see the docs:
 * - `gatsby-image`: https://gatsby.app/gatsby-image
 * - `StaticQuery`: https://gatsby.app/staticquery
 */

const Selfie = () => (
  <StaticQuery
    query={graphql`
      query {
        placeholderImage: file(
          relativePath: { eq: "RhiDesign Troy Avatar.png" }
        ) {
          childImageSharp {
            fluid(maxWidth: 700) {
              ...GatsbyImageSharpFluid
            }
          }
        }
      }
    `}
    render={data => (
      <Img
        className="rounded-lg w-1/2 max-w-sm mx-auto"
        fluid={data.placeholderImage.childImageSharp.fluid}
      />
    )}
  />
)

export default Selfie
