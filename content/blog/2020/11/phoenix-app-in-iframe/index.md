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

### Allow `iframe` Plug

To actually add this response header, we are going to leverage Phoenix's use of [Plug](https://hexdocs.pm/phoenix/plug.html). Plug is a library that allows you to interact with HTTP requests and responses in a modular manner. Multiple plugs are composed together to provide make Phoenix work. Plugs can also be conditionally used based on the request, this is something we will be able to leverage in our Plug, ensuring we only use it for endpoints related to our Connect app.  

We will create a [module plug](https://hexdocs.pm/phoenix/plug.html#module-plugs) like the following. Note the `call` function as that is where the logic lives.

```elixir{15-21}
# lib/my_app_web/plugs/allow_iframe.ex
defmodule MyAppWebb.Plugs.AllowIframe do
  @moduledoc """
  Allow rendering in iframes for Jira. Uses `Content-Security-Policy: fame-ancestors` which
  limits where the app can be loaded as an `iframe` to only specified hosts.

  See [MDN
  docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors)
  for more details.
  """
  import Plug.Conn

  def init(_), do: %{}

  def call(conn, _opts) do
    put_resp_header(
      conn,
      "content-security-policy",
      "frame-ancestors 'self' https://*.atlassian.net;"
    )
  end
end
```

In our `call` function we are using [`put_resp_header/3`](https://hexdocs.pm/plug/Plug.Conn.html?#put_resp_header/3) to update the response headers to include the `Content-Security-Policy` header with the `frame-ancestors` directive.

### Plugging it In

Now that we have a Plug, we want to add it to our [router](https://hexdocs.pm/phoenix/plug.html#controller-plugs) so it will actually be used. Plugs can also be added to your application in your [endpoint](https://hexdocs.pm/phoenix/plug.html#endpoint-plugs) or in [controllers](https://hexdocs.pm/phoenix/plug.html#controller-plugs). When building Connect applicatoin, _most_ of your Jira-related interactions with end-users will be done via an `iframe`. Since this would impact a large section of routes (but not all), I decided to add the `plug` to the router.

In order to use a Plug in the `router`, it must be [included in a pipeline](https://hexdocs.pm/phoenix/plug.html#router-plugs). Again, since there I expect a portion of the application that interacts with Jira to need to be rendered in an `iframe` using a `pipeline` made sense. 


```elixir
pipeline :jira do
  plug MyAppWeb.Plugs.AllowIframe
end
```

Now, I can group routes that will be rendered in the Jira Connect application together for logical group and to also have them all be able to be rendered in an `iframe`:

```elixir
scope "/jira", MyAppWeb.Jira, as: :jira do
  pipe_through [:browser, :jira]

  live "/", PageLive, :index
  get "/issue/:id", IssueController, :show
end
```

I also leverage Phoenix's [scope](https://hexdocs.pm/phoenix/routing.html#scoped-routes) block to group all routes to be nested under `/jira` and expect all modules to be namespaced with `MyAppWeb.Jira`. This helps organize the code.

### Testing the Plug

If we want, we could test our Plug directly:

```elixir
defmodule MyAppWeb.Plugs.AllowIframeTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias MyAppWeb.Plugs.AllowIframe

  test "adds Content-Security-Policy header with frame-ancestors directive to response headers" do
    # Create a test connection
    conn = 
      conn(:get, "/hello")
      # Invoke the plug
      |> AllowIframe.call(conn, %{})

    assert Enum.find(
             conn.resp_headers,
             fn {header, _value} -> header == "content-security-policy" end
           ) == {"content-security-policy", "frame-ancestors 'self' https://*.atlassian.net;"}
  end
end
```

This doesn't test any of my routes actually leverage this plug, but does provide a unit test-style test for the logic in the plug. For now, I am happy enough with this, but do plan to explore whether I could add the additional ease-of-mind by making sure routes I expect to be renderable in an `iframe` will work as expected.

## Cookies

At this point, we can render our application within an `iframe` in Jira (or wherever you set your `frame-ancestors`). However, when you run the app, you may find issues with the user session. In my case, I was attempting to leverage a live route, but found the page would continuously reload. 

The Phoenix server tried to helpefully log a message with the issue and even included possiblities for resolving it.

```
[debug] LiveView session was misconfigured or the user token is outdated.
```

However, it looked like the auto-generated application was set up to follow all suggestions. When looking at the server logs, I notice the `_csrf_token` was include in the parameters when attempting to connect to the socket, but this token was changing with every page reload and socket reconnect attempt. 

After some digging, I eventually found out the problem was the cookie, which is supposed to store information like the CSRF token, was not getting properly set when loading the `iframe` from within Jira. This is because the default behavior for cookies is to be first-party, meaning they are only accessible on the same domain as the server.

Since we are rendering our site in an `iframe` we are attempting to access the cookies for out application from an Atlassian/Jira domain. This is known as a third-party cookie. For security reasons, this is blocked by default and requires explicitly setting the [`SameSite`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite) property of your cookie to `'None'`. When sending cookies to third-parties, you must also set the [`Secure`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#Secure) property on the cookie. This indicates that the cookie should only be accessible when the requesting site is using `https` (or is `localhost`). For more information on the `SameSite` options when working with cookies check out [this article](https://web.dev/samesite-cookies-explained/).

### Thid Party Cookies with Phoenix

Updating you cookie settings

https://hexdocs.pm/phoenix/plug.html#endpoint-plugs
https://hexdocs.pm/plug/Plug.Session.html


 Outline

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
  * https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite
& https://developer.atlassian.com/cloud/jira/platform/#atlassian-connect
& https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors
