---
title: 'Running a Phoenix App in an iframe'
date: '2020-11-14T05:31:13.265Z'
categories: ['elixir', 'phoenix']
---

For a side project, I am working on building a [Jira Connect application](https://developer.atlassian.com/cloud/jira/platform/#atlassian-connect) using [Elixir](https://elixir-lang.org/) and [Phoenix](https://www.phoenixframework.org/). When building a Connect app, you run run your own server, but your UI is rendered within Jira as though it is a part of Jira itself. This in-Jira UI rendering is done via an [`iframe`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe).

To get this to work securely with my Phoenix app, I needed to:

1. Update the `Content-Security-Policy` response headers to allow some of the application's pages to be embedded in an `iframe`.
2. Update the `SameSite` settings for the session cookie to allow the cookies to be used by a third-party.

## `Content-Security-Policy`

The [`Content-Security-Policy` headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy) provide a mechanmism for the server to tell the browser what content is safe to load. A common use case for this is to indicate which third-party JavaScript is safe to load, blocking the rest and preventing [XSS attacks](https://developer.mozilla.org/en-US/docs/Glossary/Cross-site_scripting). In this case, we want to specify where it is okay to render our `iframe`; for this, we use the [`frame-ancestors`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors) directive. This is similar to the older [`X-Frame-Option`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options) response header.

The `fame-ancestors` directive expects a list of sources. Sources can be hostnames, IP addresses, or URLs, and can include wildcard matchers (`*`). There are also a few special case sources, `'none'` which indicates the page is not allowed to be renderd in an `iframe` and `'self'` which allows you to render the content in an `iframe` if the request comes from the same `origin`.

For the purposes of the Connect app I was building, I wanted to allow the `iframe` to be rendered in any cloud instance of Jira, the ability to use wildcards was perfect for this. My goal was to end up with something like:

```html
Content-Security-Policy: frame-ancestors 'self' https://*.atlassian.net
```

In addition to allowing the content to be rendered in an `iframe` on any Atlassian subdomain, this also includes the `'self'` source. This was included to allow for manual testing. Depending on your needs and testing strategy, you may be able to remove it.

# Outline

* Jira connect app
* Be selective about when to show up in an iframe (`plug`)
    * Create a plug that allows atlassian
    ```elixir
    # lib/your_app_web/plug/allow_iframe.ex

      def call(conn, _opts) do
        put_resp_header(
          conn,
          "content-security-policy",
          "frame-ancestors 'self' https://*.atlassian.net;"
        )
      end
    ```
      * TODO I think there was a blog post that mentioned an alternative way to check if you should allow the iframe that allowed for more logic
    * Enable the plug for routs that should hit your iframe
        ```elixir
          pipeline :jira do
            plug BetterEstimatorWeb.Jira.Plugs.AllowIframe
            plug BetterEstimatorWeb.Jira.Plugs.JWT
            plug :put_root_layout, {BetterEstimatorWeb.LayoutView, :jira}
          end
        ```

* Work with Phoenix cookies (https://medium.com/trabe/cookies-and-iframes-f7cca58b3b9e) 
    ```elixir{6-7}
      # lib/your_app_web/endpoint.ex
      @session_options [
      store: :cookie,
      key: "_your_app_key",
      signing_salt: "salt",
      secure: true,
      same_site: "None"
    ]
    ```

## Sources to use and reference

* https://web.dev/samesite-cookies-explained/
    * https://web.dev/samesite-cookie-recipes/
& https://developer.atlassian.com/cloud/jira/platform/#atlassian-connect
& https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors
