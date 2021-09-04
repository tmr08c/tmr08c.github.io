import { Link, StaticQuery, graphql } from "gatsby";
import * as React from "react";

function Header(): JSX.Element {
  return (
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
      render={(data) => (
        <nav className="flex items-center justify-between flex-wrap bg-green-800 px-6 py-12 mb-5 text-white">
          <div className="flex flex-no-shrink">
            <Link
              to="/"
              className="font-semibold text-4xl tracking-tighter hover:underline hover:animate-wiggle"
            >
              {data.site.siteMetadata.title}
            </Link>
          </div>
          <div className="justify-end flex mr-4">
            <div className="text-xl">
              <Link to="/talks" className="mr-6 hover:text-black">
                Talks
              </Link>

              <Link to="/blog" className="hover:text-black">
                Blog
              </Link>
            </div>
          </div>
        </nav>
      )}
    />
  );
}

export default Header;
