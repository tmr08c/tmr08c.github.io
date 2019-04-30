import { graphql, StaticQuery } from 'gatsby'
import * as React from 'react'

import Footer from './Footer'
import Header from './Header'

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
          className="min-h-screen"
          style={{
            margin: '0 auto',
            maxWidth: '40rem',
            padding: '0px 1.0875rem 1.45rem',
            paddingTop: 0,
          }}
        >
          {children}
        </div>
        <Footer />
      </>
    )}
  />
)

export default Layout
