import { Link } from 'gatsby'
import * as React from 'react'

interface HeaderProps {
  siteTitle?: string
}

const Header: React.SFC<HeaderProps> = ({ siteTitle }) => (
  <nav
    className={
      'flex items-center justify-between flex-wrap bg-purple-800 p-6 shadow mb-5'
    }
  >
    <div className={'flex flex-no-shrink'}>
      <Link
        to="/"
        className={
          'font-semibold text-2xl tracking-tighter text-yellow-400 border-b-4 border-transparent border-dashed hover:border-yellow-400'
        }
      >
        {siteTitle}
      </Link>
    </div>
    <div className={'justify-end flex mr-4'}>
      <div className={'text-md'}>
        <Link to="/blog" className={'text-yellow-400 hover:text-white'}>
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
