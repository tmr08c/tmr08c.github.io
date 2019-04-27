import { graphql, Link } from 'gatsby'
import * as React from 'react'
import Selfie from '../components/Selfie'
import SEO from '../components/seo'
import Header from '../components/Header'

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
      <div className="object-cover min-h-screen bg-purple-800 ">
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

        <div className="flex flex-col justify-center text-center text-white">
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
