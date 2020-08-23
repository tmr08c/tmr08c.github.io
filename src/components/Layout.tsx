import { graphql, StaticQuery } from "gatsby";
import * as React from "react";

import Footer from "./Footer";
import Header from "./Header";

const Layout: React.SFC = ({ children }) => (
  <StaticQuery
    query={graphql`
      query SiteTitleQuery {
        site {
          siteMetadata {
            title
          }
        }
      }
    `}
    render={data => (
      <>
        <Header siteTitle={data.site.siteMetadata.title} />
        <div
          className="min-h-screen max-w-3xl mx-auto px-4 sm:px-6 xl:max-w-5xl xl:px-0"
        >
          {children}
        </div>
        <Footer />
      </>
    )}
  />
);

export default Layout;
