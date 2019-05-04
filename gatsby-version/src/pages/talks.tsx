import * as React from 'react'
import Layout from '../components/layout'
import SEO from '../components/seo'
import YearList from '../components/talks/YearList'

class TalksPage extends React.Component<{}, {}> {
  talkList: TroyProgBlog.TalkList = {
    '2013': [
      {
        description: 'not much happened here, it was the first go of it',
        name: 'First talk',
        presentationDate: { year: 2013, month: 3 },
      },
    ],
    '2014': [
      {
        description: 'des2',
        name: 'Some talking',
        presentationDate: { year: 2014, month: 3 },
      },
      {
        description: 'des3',
        name: 'A talk about some things',
        presentationDate: { year: 2014, month: 3 },
      },
    ],
  }

  render() {
    return (
      <Layout>
        <SEO title="Talks" keywords={['programming', 'conference talks']} />
        <h1 className={'text-5xl text-center font-bold mb-5'}>Talks</h1>
        {Object.keys(this.talkList)
          .sort()
          .reverse()
          .map(year => {
            return (
              <YearList key={year} year={year} talks={this.talkList[year]} />
            )
          })}
      </Layout>
    )
  }
}

export default TalksPage
