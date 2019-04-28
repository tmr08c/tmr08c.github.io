import { Link } from 'gatsby'
import * as React from 'react'

interface HeaderProps {
  siteTitle?: string
}

const Header: React.SFC<HeaderProps> = ({ siteTitle }) => (
  <nav className="flex items-center justify-between flex-wrap bg-living-coral-500 p-6 mb-5">
    <div className="flex flex-no-shrink">
      <Link
        to="/"
        className="font-semibold text-xl tracking-tighter text-black hover:text-white"
      >
        {siteTitle}
      </Link>
    </div>
    <div className="justify-end flex mr-4">
      <div className="text-xl">
        <a
          href="https://github.com/tmr08c/"
          target="blank"
          className="text-black hover:text-white mx-2"
        >
          GitHub
        </a>
        <Link to="/blog" className="text-black hover:text-white">
          Blog
        </Link>
      </div>
    </div>
  </nav>
)

Header.defaultProps = {
  siteTitle: '',
}

export default Header
