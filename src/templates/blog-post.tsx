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

interface PostInfo {
    frontmatter: {
        title: string;
    };
    fields: {
        slug: string;
    };
}

const EditOnGitHubLink = ({
    repoLink,
    postSlug,
}: {
    repoLink: string;
    postSlug: string;
}) => {
    const linkToPostInRepo = `${repoLink}/tree/develop/content/blog/${postSlug}index.md`;
    const linkToIssues = `${repoLink}/issues`;

    return (
        <div className="text-center font-light text-sm md:text-sm text-gray-600 italic mb-10 xl:mb-8 dark:text-gray-300">
            Notice something wrong? Please consider{" "}
            <a
                href={linkToPostInRepo}
                target="_blank"
                rel="noopener noreferrer"
                className="underline hover:text-purple-400"
            >
                proposing an edit
            </a>{" "}
            or{" "}
            <a
                href={linkToIssues}
                target="_blank"
                rel="noopener noreferrer"
                className="underline hover:text-purple-400"
            >
                opening an issue
            </a>
            .
        </div>
    );
};

const OtherPostsNav = ({
    previous,
    next,
}: {
    previous: PostInfo;
    next: PostInfo;
}) => {
    return (
        <div className="text-gray-600 grid grid-flow-col grid-cols-3 justify-items-stretch align-middle items-center text-xs sm:text-sm lg:text-base dark:text-gray-300">
            {previous ? (
                <Link
                    to={previous.fields.slug}
                    rel="prev"
                    className="flex flex-row hover:text-purple-400"
                >
                    <span className="self-center flex-none mr-2">←</span>
                    <span>{previous.frontmatter.title}</span>
                </Link>
            ) : (
                <span></span>
            )}
            <Link to="/blog" className="text-center hover:text-purple-400">
                - All Posts -
            </Link>
            {next ? (
                <Link
                    to={next.fields.slug}
                    rel="next"
                    className="flex flex-row justify-right hover:text-purple-400"
                >
                    <span className="text-right">{next.frontmatter.title}</span>
                    <span className="self-center flex-none ml-2">→</span>
                </Link>
            ) : (
                <span></span>
            )}
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
                <article className="prose dark:prose-light mx-auto">
                    <SEO title={post.frontmatter.title} description={post.excerpt} />
                    <h1>{post.frontmatter.title}</h1>
                    <div dangerouslySetInnerHTML={{ __html: post.html }} />
                </article>

                <div className="flex justify-end text-xs md:text-sm text-gray-600 font-light italic mb-2 dark:text-gray-400">
                    <time dateTime={post.frontmatter.date}>{post.frontmatter.date}</time>
                </div>

                <hr className="mb-3" />

                <EditOnGitHubLink repoLink={repoLink} postSlug={slug} />
                <OtherPostsNav previous={previous} next={next} />
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
