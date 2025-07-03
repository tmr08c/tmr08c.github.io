/// <reference types="cypress" />

describe("Blog Posts", () => {
  beforeEach(() => {
    cy.visit("/blog");
  });

  describe("visiting index page", () => {
    it("lists posts", () => {
      cy.exec("find ../hugo-site/content/blog -type f -name index.md | wc -l").then(
        (result) => {
          const numPosts = parseInt(result.stdout);

          assert.isTrue(numPosts > 0);
          cy.get(".post-info").should("have.length", numPosts);
        }
      );
    });
  });

  describe("clicking on a post title", () => {
    it("takes you to the post", () => {
      cy.get(".post-info").each(($post) => {
        const link = $post.find("a.link").first();
        const title = link.text();

        // Note: For CI we needed to add `"chromeWebSecurity": false` to get
        // visiting other pages to work
        cy.visit(link.attr("href")).get("h1").should("contain.text", title);

        cy.go("back");
      });
    });
  });
});
