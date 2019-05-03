import { graphql, Link } from 'gatsby'
import * as React from 'react'
import Footer from '../components/Footer'
import Header from '../components/Header'
import Selfie from '../components/Selfie'
import SEO from '../components/seo'

interface PostNode {
  node: {
    excerpt: string
    frontmatter: {
      date: string
      title: string
    }
    fields: {
      slug: string
    }
  }
}

interface IndexPageProps {
  data: {
    site: {
      siteMetadata: {
        siteName: string
      }
    }
  }
}

class IndexPage extends React.Component<IndexPageProps, {}> {
  render() {
    const { data } = this.props

    return (
      <>
        <div className="object-cover min-h-screen bg-living-coral-500">
          <SEO
            title="Home"
            keywords={[
              'blog',
              'gatsby',
              'javascript',
              'typescript',
              'react',
              'programming',
              'ruby',
              'rails',
              'elixir',
              'phoenix',
              'software development',
              'engineering manager',
              'code',
            ]}
          />

          <Header />

          <div className="flex flex-col justify-center text-center text-black">
            <h1
              className="text-5xl lg:text-8xl p-10 pb-4 "
              style={{ fontFamily: 'Chewy' }}
            >
              Hello, I'm Troy!
            </h1>

            <Selfie />

            <h2 className="text-xl lg:text-2xl font-hairline p-6">
              I am a Software Developer &amp; Engineering Manager
            </h2>
          </div>
        </div>

        <Footer />
      </>
    )
  }
}

export default IndexPage

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        title
      }
    }
  }
`
