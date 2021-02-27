/// <reference types="cypress" />

describe('Blog Posts', () => {
  beforeEach(() => {
    cy.visit('/blog')
  })

  describe('visiting index page', () => {
    it('lists posts', () => {
      cy.exec("find content/blog -type f -name index.md | wc -l").then((result) => {
        const numPosts = parseInt(result.stdout)

        assert.isTrue(numPosts > 0)
        cy.get('.posts .post').should('have.length', numPosts)
      })
    })
  })

  describe('clicking on a post title', () => {
    it('takes you to the post', () => {
      cy.get('.posts .post').each(($post) => {
        const link = $post.find('a').first()
        const title = link.text()

        cy.visit(link.attr('href'))

        cy.get('h1').should('contain.text', title)

        cy.go('back')
      })
    })
  })
})
