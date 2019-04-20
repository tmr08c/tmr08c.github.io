import { graphql, Link } from 'gatsby'
import * as React from 'react'
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
      <div
        className="object-cover min-h-screen flex flex-col justify-center"
        style={{
          background: 'linear-gradient(green, purple)',
        }}
      >
        <SEO
          title="All posts"
          keywords={['blog', 'gatsby', 'javascript', 'react']}
        />
        <h1 className="text-white text-5xl text-center p-10">
          Hello, I'm Troy
        </h1>
        <Selfie />
        <h2 className="text-white text-3xl text-center p-10">
          I am a Software Developer &amp; Engineering Manager
        </h2>
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
