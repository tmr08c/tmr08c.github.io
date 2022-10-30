/// <reference types="cypress" />

describe("Homepage", () => {
  beforeEach(() => {
    cy.visit("/");
  });

  describe("content", () => {
    it("says something about me", () => {
      cy.get("body").should("contain", "Troy");
    });
  });

  describe("navigation", () => {
    it("can navigate to blog posts", () => {
      cy.get("nav").contains("Blog").click();
      cy.location("pathname").should("include", "blog");
    });

    it("can navigate to talks", () => {
      cy.get("nav").contains("Talks").click();
      cy.location("pathname").should("include", "talks");
    });
  });
});
