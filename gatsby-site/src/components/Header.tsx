import { Link, StaticQuery, graphql } from "gatsby";
import * as React from "react";

import DarkModeToggle from "./DarkModeToggle";

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
        <nav className="flex items-center justify-between flex-wrap bg-green-800 px-6 py-12 mb-5 text-white dark:bg-gray-800 dark:border-b-4 dark:border-double dark:border-purple-400 dark:text-purple-400">
          <div className="flex flex-no-shrink">
            <Link
              to="/"
              className="font-semibold text-4xl tracking-tighter hover:underline hover:animate-wiggle"
            >
              {data.site.siteMetadata.title}
            </Link>
          </div>
          <div className="justify-end flex mr-4 text-xl items-center">
            <Link
              to="/talks"
              className="mr-6 hover:text-black dark:hover:text-white"
            >
              Talks
            </Link>

            <Link
              to="/blog"
              className="mr-6 hover:text-black dark:hover:text-white"
            >
              Blog
            </Link>

            <DarkModeToggle />
          </div>
        </nav>
      )}
    />
  );
}

export default Header;
