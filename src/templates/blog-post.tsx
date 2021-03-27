import { graphql, Link } from "gatsby";
import * as React from "react";
import Layout from "../components/Layout";
import SEO from "../components/SEO";

interface BlogPostTemplateProps {
  data: {
    site: {
      siteMetadata: {
        repsitory: string;
      };
    };
    markdownRemark: {
      id: string;
      html: string;
      excerpt: string;
      frontmatter: {
        title: string;
        date: string;
      };
    };
  };
  pageContext: {
    previous: any;
    next: any;
    slug: string;
  };
}

const EditOnGitHubLink = ({
  repoLink,
  postSlug
}: {
  repoLink: string;
  postSlug: string;
}) => {
  const linkToPostInRepo = `${repoLink}/tree/develop/content/blog/${postSlug}index.md`;
  const linkToIssues = `${repoLink}/issues`;

  return (
    <div className="text-center font-light text-sm md:text-sm text-gray-600 italic mb-3">
      Notice something wrong? Please consider{" "}
      <a
        href={linkToPostInRepo}
        target="_blank"
        rel="noopener noreferrer"
        className="underline hover:text-living-coral-500"
      >
        proposing an edit
      </a>{" "}
      or{" "}
      <a
        href={linkToIssues}
        target="_blank"
        rel="noopener noreferrer"
        className="underline hover:text-living-coral-500"
      >
        opening an issue
      </a>
      .
    </div>
  );
};

class BlogPostTemplate extends React.Component<BlogPostTemplateProps, {}> {
  render() {
    const post = this.props.data.markdownRemark;
    const { slug, previous, next } = this.props.pageContext;
    const repoLink = this.props.data.site.siteMetadata.repsitory;

    return (
      <Layout>
        <article
            className="prose prose-sm sm:prose lg:prose-lg xl:prose-xl"
        >
          <SEO title={post.frontmatter.title} description={post.excerpt} />
          <h1>{post.frontmatter.title}</h1>
          <div
            dangerouslySetInnerHTML={{ __html: post.html }}
          />
        </article>

        <div className="flex justify-end text-xs md:text-sm text-gray-600 font-light italic mb-2">
          <time dateTime={post.frontmatter.date}>
            {post.frontmatter.date}
          </time>
        </div>

        <hr className="mb-3" />

        <EditOnGitHubLink repoLink={repoLink} postSlug={slug} />

        <ul className="text-gray-600 flex flex-wrap justify-between">
          <li>
            {previous && (
              <Link to={previous.fields.slug} rel="prev">
                ← {previous.frontmatter.title}
              </Link>
            )}
          </li>
          <li>
            <Link to="/blog">- All Posts -</Link>
          </li>
          <li>
            {next && (
              <Link to={next.fields.slug} rel="next">
                {next.frontmatter.title} →
              </Link>
            )}
          </li>
        </ul>
      </Layout>
    );
  }
}

export default BlogPostTemplate;

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    site {
      siteMetadata {
        repsitory
      }
    }
    markdownRemark(fields: { slug: { eq: $slug } }) {
      id
      excerpt(pruneLength: 160)
      html
      frontmatter {
        title
        date(formatString: "MMMM DD, YYYY")
      }
    }
  }
`;
