import { Link } from "gatsby";
import * as React from "react";

function Footer() {
  return (
    <div className="flex justify-between py-4 px-1 bg-green-800 text-white dark:bg-gray-800 dark:text-purple-400">
      <div className="flex flex-col pl-5 my-auto">
        <Link to="/" className="hover:text-black dark:hover:text-white">
          Home
        </Link>
        <div className="group">
          <a
            href="https://github.com/tmr08c/"
            target="blank"
            className="group-hover:text-black dark:group-hover:text-white"
          >
            {/* GitHub Logo SVG generated from Jekyll version of blog */}
            <svg viewBox="0 0 16 16" className="w-5 h-5 inline fill-current">
              <path d="M7.999,0.431c-4.285,0-7.76,3.474-7.76,7.761 c0,3.428,2.223,6.337,5.307,7.363c0.388,0.071,0.53-0.168,0.53-0.374c0-0.184-0.007-0.672-0.01-1.32 c-2.159,0.469-2.614-1.04-2.614-1.04c-0.353-0.896-0.862-1.135-0.862-1.135c-0.705-0.481,0.053-0.472,0.053-0.472 c0.779,0.055,1.189,0.8,1.189,0.8c0.692,1.186,1.816,0.843,2.258,0.645c0.071-0.502,0.271-0.843,0.493-1.037 C4.86,11.425,3.049,10.76,3.049,7.786c0-0.847,0.302-1.54,0.799-2.082C3.768,5.507,3.501,4.718,3.924,3.65 c0,0,0.652-0.209,2.134,0.796C6.677,4.273,7.34,4.187,8,4.184c0.659,0.003,1.323,0.089,1.943,0.261 c1.482-1.004,2.132-0.796,2.132-0.796c0.423,1.068,0.157,1.857,0.077,2.054c0.497,0.542,0.798,1.235,0.798,2.082 c0,2.981-1.814,3.637-3.543,3.829c0.279,0.24,0.527,0.713,0.527,1.437c0,1.037-0.01,1.874-0.01,2.129 c0,0.208,0.14,0.449,0.534,0.373c3.081-1.028,5.302-3.935,5.302-7.362C15.76,3.906,12.285,0.431,7.999,0.431z" />
            </svg>
            <span className="text-sm ml-1 group-hover:text-black dark:group-hover:text-white">
              @tmr08c
            </span>
          </a>
        </div>
      </div>
      <div className="flex flex-col items-end pr-5 my-auto">
        <Link to="/talks" className="hover:text-black dark:hover:text-white">
          Talks
        </Link>
        <div className="flex items-center">
          <Link
            to="/rss.xml"
            className="mr-3 hover:text-black dark:hover:text-white"
          >
            {/* Logo thanks to https://www.svgrepo.com/svg/95552/rss-sign  */}
            <svg viewBox="0 0 461.432 461.432" className="h-3 w-3 fill-current">
              <defs />
              <path d="M125.896 398.928c0 33.683-27.308 60.999-61.022 60.999-33.684 0-61.006-27.316-61.006-60.999 0-33.729 27.322-61.038 61.006-61.038 33.714 0 61.022 27.308 61.022 61.038zM0 229.636c0 8.441 6.606 15.379 15.036 15.809 60.318 3.076 100.885 25.031 138.248 62.582 36.716 36.864 60.071 89.759 64.082 137.769.686 8.202 7.539 14.524 15.77 14.524h56.701c4.344 0 8.498-1.784 11.488-4.935a15.852 15.852 0 004.333-11.729c-8.074-158.152-130.669-278.332-289.013-286.23a15.846 15.846 0 00-11.709 4.344A15.848 15.848 0 000 173.247v56.389z" />
              <path d="M0 73.411c0 8.51 6.713 15.482 15.216 15.819 194.21 7.683 350.315 161.798 358.098 355.879.34 8.491 7.32 15.208 15.818 15.208h56.457c4.297 0 8.408-1.744 11.393-4.834a15.857 15.857 0 004.441-11.552C453.181 199.412 261.024 9.27 16.38 1.121A15.844 15.844 0 004.838 5.568 15.842 15.842 0 000 16.954v56.457z" />
            </svg>
          </Link>
          <Link to="/blog" className="hover:text-black dark:hover:text-white">
            Blog
          </Link>
        </div>
      </div>
    </div>
  );
}
export default Footer;
