import { graphql, Link } from "gatsby";
import * as React from "react";
import Layout from "../components/Layout";
import SEO from "../components/SEO";

interface PostNode {
  node: {
    excerpt: string;
    frontmatter: {
      date: string;
      title: string;
    };
    fields: {
      slug: string;
    };
  };
}

interface IndexPageProps {
  data: {
    allMarkdownRemark: {
      edges: PostNode[];
    };
  };
}

class IndexPage extends React.Component<IndexPageProps, {}> {
  render() {
    const { data } = this.props;
    const posts = data.allMarkdownRemark.edges;

    return (
      <Layout>
        <SEO
          title="Blog"
          keywords={["blog", "gatsby", "javascript", "react"]}
        />

        <h1 className={"text-5xl text-center font-bold mb-5"}>
          All Blog Posts
        </h1>

        {posts.map(({ node }) => {
          const title = node.frontmatter.title || node.fields.slug;
          return (
            <div className={"mb-5"} key={node.fields.slug}>
              <h3 className={"text-3xl"}>
                <Link to={node.fields.slug}>{title}</Link>
              </h3>
              <small className={"italic text-gray-800"}>
                {node.frontmatter.date}
              </small>
              <p dangerouslySetInnerHTML={{ __html: node.excerpt }} />
            </div>
          );
        })}
      </Layout>
    );
  }
}

export default IndexPage;

export const pageQuery = graphql`
  query {
    allMarkdownRemark(sort: { fields: [frontmatter___date], order: DESC }) {
      edges {
        node {
          excerpt
          fields {
            slug
          }
          frontmatter {
            date(formatString: "MMMM DD, YYYY")
            title
          }
        }
      }
    }
  }
`;
