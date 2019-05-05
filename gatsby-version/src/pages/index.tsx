import * as React from 'react'
import Footer from '../components/Footer'
import Header from '../components/Header'
import HomePageLink from '../components/home/HomePageLink'
import Selfie from '../components/Selfie'
import SEO from '../components/seo'
import AboutBox from '../components/home/AboutBox'

class IndexPage extends React.Component<{}, {}> {
  render() {
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

        <div className="flex flex-col lg:flex-row lg:justify-around bg-living-coral-500">
          <AboutBox
            customStyle={{ backgroundColor: '#FCE21A', color: '#1e3859' }}
            sectionTitle="Work"
          >
            <p className="my-2">
              I am a full-time Engineering Manger and Software Developer mostly
              working with open-source web technologies including
              <span className="text-xl ml-1" style={{ color: '#284E80' }}>
                TypeScript
              </span>
              ,
              <span className="text-xl ml-1" style={{ color: '#4e2a8e' }}>
                Elixir
              </span>
              , and
              <span className="text-xl ml-1" style={{ color: '#A51601' }}>
                Ruby
              </span>
              .
            </p>
            <p className="my-2">
              In addition to working with different languages and frameworks, I
              like testing
              <HomePageLink
                destination="https://www.vim.org/"
                displayText="different"
              />
              <HomePageLink
                destination="https://code.visualstudio.com/"
                displayText="text"
              />
              <HomePageLink
                destination="http://spacemacs.org/"
                displayText="editors"
              />
              .
            </p>
            <p className="my-2">
              You can find some of my work on
              <HomePageLink
                destination="https://github.com/tmr08c/"
                displayText="GitHub"
              />
              .
            </p>
          </AboutBox>

          <AboutBox
            customStyle={{ backgroundColor: '#50e1c7', color: '#1E3859' }}
            sectionTitle="Home"
          >
            <p className="my-2">
              Whether working or not, I am generally well caffeinated and use
              almost as many
              <HomePageLink
                destination="https://aeropress.com/"
                displayText="coffee"
              />
              <HomePageLink
                destination="https://www.chemexcoffeemaker.com/"
                displayText="brewing"
              />
              <HomePageLink
                destination="https://www.hario.jp/sp_v60series.html"
                displayText="options"
              />
              &nbsp;as I do text editors.
            </p>
            <p className="my-2">
              When I'm not working, you can usually find me listening to
              podcasts, walking my dogs, rock climbing, or spending time my
              <HomePageLink
                destination="http://rhimerchant.com/"
                displayText="amazing girlfriend"
              />
              &nbsp;who created my awesome avatar for me.
            </p>
          </AboutBox>
        </div>

        <div className="bg-living-coral-500 h-32" />

        <Footer />
      </>
    )
  }
}

export default IndexPage
