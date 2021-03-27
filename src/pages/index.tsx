import * as React from "react";
import Footer from "../components/Footer";
import Header from "../components/Header";
import HomePageLink from "../components/home/HomePageLink";
import Selfie from "../components/Selfie";
import SEO from "../components/SEO";
import AboutBox from "../components/home/AboutBox";

class IndexPage extends React.Component<{}, {}> {
  render() {
    return (
      <>
        <div className="object-cover min-h-screen bg-green-800">
          <SEO
            title="Home"
            keywords={[
              "blog",
              "gatsby",
              "javascript",
              "typescript",
              "react",
              "programming",
              "ruby",
              "rails",
              "elixir",
              "phoenix",
              "software development",
              "engineering manager",
              "code"
            ]}
          />

          <Header />

          <div className="flex flex-col justify-center text-center text-white">
            <h1
              className="text-5xl lg:text-8xl p-10 pb-4 "
              style={{ fontFamily: "Chewy" }}
            >
              Hello, I'm Troy!
            </h1>

            <Selfie />

            <h2 className="text-xl lg:text-2xl font-thin p-6">
              I am a Software Developer &amp; Engineering Manager
            </h2>
          </div>
        </div>

        <Footer />
      </>
    );
  }
}

export default IndexPage;
