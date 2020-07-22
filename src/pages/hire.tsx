import * as React from "react";
import SEO from "../components/SEO";
import Layout from "../components/Layout";

class HirePage extends React.Component<{}, {}> {
  render() {
    return (
      <Layout>
        <SEO
          title="Hire Me"
          keywords={[ ]} />

        <div className="flex flex-col justify-center text-black">
          <h1 className="text-5xl lg:text-5xl p-10 pb-4 text-center">
            Want to hire me?
          </h1>

          <h2>Need help building or maintaining a web app?</h2>

          <p>
            I have experience building and maintaining systems with Ruby, TypeScript, and Elixir and popular frameworks and libraries like Rails, React (Gatsby, NextJS), and Phoenix. 
          </p>

          <h2>Want to level up your career?</h2>

          <p>
            In addition to contract development work, I provide professional coaching and mentorship. I can leverage my experience as both engineering manager and individual contributor to help guide you on your journey to becoming a better software developer.
          </p>
        </div>


      </Layout>
    );
  }
}

export default HirePage;
