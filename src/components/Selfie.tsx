import { graphql, StaticQuery } from "gatsby";
import { StaticImage } from "gatsby-plugin-image";
import * as React from "react";

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

const Selfie = () => {
  return (
    <StaticImage
      src="../images/RhiDesign Troy Avatar.png"
      alt="avatar"
      className="rounded-lg w-1/2 max-w-sm mx-auto"
    />
  );
};

export default Selfie;
