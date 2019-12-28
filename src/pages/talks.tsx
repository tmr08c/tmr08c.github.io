import { graphql } from 'gatsby'
import * as React from 'react'
import Layout from '../components/Layout'
import SEO from '../components/SEO'
import YearList from '../components/talks/YearList'

interface TalkNode {
  node: {
    year: string
    talks: TroyProgBlog.Talk[]
  }
}

interface TalkPageProps {
  data: {
    allTalksJson: {
      edges: TalkNode[]
    }
  }
}

class TalksPage extends React.Component<TalkPageProps, {}> {
  render() {
    const {
      data: {
        allTalksJson: { edges: talkListEdges },
      },
    } = this.props

    return (
      <Layout>
        <SEO title="Talks" keywords={['programming', 'conference talks']} />
        <h1 className={'text-5xl  font-bold mb-5'}>Talks</h1>
        <div className="text-md text-gray-600 italic mb-8">
          Below are some talks that I have given over time. When possible, I
          have provided links to slides and/or code.
        </div>
        {talkListEdges.map(({ node: { year, talks } }) => {
          return <YearList key={year} year={year} talks={talks} />
        })}
      </Layout>
    )
  }
}

export default TalksPage

export const pageQuery = graphql`
  query {
    allTalksJson(sort: { fields: year, order: DESC }) {
      edges {
        node {
          year
          talks {
            description
            link
            name
            presentationDate {
              year
              month
            }
          }
        }
      }
    }
  }
`
