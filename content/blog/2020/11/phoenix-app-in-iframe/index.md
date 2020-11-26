---
title: 'Running a Phoenix App in an iframe'
date: '2020-11-14T05:31:13.265Z'
categories: ['elixir', 'phoenix']
---

For a side project, I am working on building a [Jira Connect application](https://developer.atlassian.com/cloud/jira/platform/#atlassian-connect) using [Elixir](https://elixir-lang.org/) and [Phoenix](https://www.phoenixframework.org/). With a Connect application, your UI is rendered within Jira as though it is a part of Jira itself. This in-Jira UI rendering is supported by the use of [`iframe`s](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe).

To get this to work securely with my Phoenix application, I needed to:

1. Update the `Content-Security-Policy` response headers to allow some pages to be embedded in an `iframe`
2. Update the `SameSite` settings for the session cookie to allow the cookies to be used by a third-party

In this post, I will walk through how I set that up for this project.

## `Content-Security-Policy`

The [`Content-Security-Policy` headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy) provide a mechanism for the server to tell the browser what content is safe to load. A common use case for this is to help prevent [XSS attacks](https://developer.mozilla.org/en-US/docs/Glossary/Cross-site_scripting)
by blocking all JavaScript not explicitly listed in the `Content-Security-Policy`.

In this case, we want to specify where it is okay to render our `iframe`. For this, we use the [`frame-ancestors`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors) directive.

The `fame-ancestors` directive expects a list of sources (e.g., URLs) that should be allowed to render the content in an `iframe`. For the Connect application, I wanted to allow the `iframe` to be renderable in any cloud instance of Jira. To do this, I was able to leverage the ability to use wildcard matchers to match any subdomain of `atlassian.net` - `https://*.atlassian.net`.

I also decided to include the [`'self'`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-src) source. This source enables `iframe`s to be renderable when the `iframe`'s `src` matches the origin of the page that is rendering it. For now, I am using this for manual testing, but, depending on your needs and testing strategy, you may be able to remove it.

With these two needs in mind, my goal was to end up with a `Content-Security-Policy` header that matched:

```html
Content-Security-Policy: frame-ancestors 'self' https://*.atlassian.net
```

### Allow `iframe` Plug

To add this response header, we are going to utilize Phoenix's use of [Plug](https://hexdocs.pm/phoenix/plug.html). Plug is a library that allows you to interact with HTTP requests and responses in a modular manner and makes up the core of Phoenix's HTTP lifecycle.

### Testing the Plug

We can use the [`Plug.Test`](https://hexdocs.pm/plug/Plug.Test.html) module to test-drive that the plug we create will have the `Content-Security-Policy` we expect.

```elixir
# test/my_app_web/plugs/allow_iframe_test.exs
defmodule MyAppWeb.Plugs.AllowIframeTest do
  use ExUnit.Case, async: true
  use Plug.Test

  alias MyAppWeb.Plugs.AllowIframe

  test "updates Content-Security-Policy headers" do
    # Create a test connection
    conn =
      conn(:get, "/hello")
      # Invoke the plug
      |> AllowIframe.call(%{})

    # Get the `Content-Security-Policy`
    # from the response headers
    csp_headers =
      Enum.find(
        conn.resp_headers,
        fn {header, _value} ->
          header == "content-security-policy"
        end
      )

    # Confirm it includes the `frame-ancestors` directive
    # with 'self' and our Atlassian wildcard matcher
    assert(
      csp_headers == {
        "content-security-policy",
        "frame-ancestors 'self' https://*.atlassian.net;"
      }
    )
  end
end
```

This test will create a `conn` and pass that into our soon-to-be plug. It then `assert`s that our `conn` will have response headers that include our expected `Content-Security-Policy` and `frame-ancestors` pairing.

With our failing test in place, we can now create a plug that will make it pass.

### Creating the Plug

To implement our `AllowIframe` plug, we will create a [module plug](https://hexdocs.pm/phoenix/plug.html#module-plugs).

In our `call` function we will use [`put_resp_header/3`](https://hexdocs.pm/plug/Plug.Conn.html?#put_resp_header/3) to update the response headers to include the `Content-Security-Policy` header with the `frame-ancestors` directive. The end result should look something like: 

```elixir{16-21}
# lib/my_app_web/plugs/allow_iframe.ex
defmodule MyAppWebb.Plugs.AllowIframe do
  @moduledoc """
  Allow rendering in iframes for Jira.
  Uses `Content-Security-Policy: fame-ancestors`
  which limits where the app can be loaded as
  an `iframe` to only specified hosts.

  See [MDN docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors)
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

### Plugging it In

For our use case, we are going to add our plug to the [router](https://hexdocs.pm/phoenix/plug.html#router-plugs). Router plugs allow us to manage their usage on a per-route level instead of a [per-controller](https://hexdocs.pm/phoenix/plug.html#controller-plugs) or more [global (endpoint)](https://hexdocs.pm/phoenix/plug.html#endpoint-plugs) basis. 

To use a plug in the `router` it must be [included in a pipeline](https://hexdocs.pm/phoenix/plug.html#router-plugs). Since we are building a Connect application, I decided to make a `pipeline` to group routes that will be renderable within the Jira UI.

```elixir
# This pipeline should be used for routes
# that are expected to be requested as
# a part of the Jira Connect application
pipeline :jira do
  plug MyAppWeb.Plugs.AllowIframe
end
```

I did not add this directly to the existing `browser` `pipeline` because I anticipated the need to have some routes that would not be required to render within the Jira UI (and therefore not in an `iframe`). While your needs will be different, please remember the ability to render `iframe`s is limited for security purposes. I would suggest trying to limit the ability to display your application in an `iframe` as much as possible and allow access on an as-needed basis instead of defaulting to making it available.

With the `pipeline` in place, I can group routes that will be rendered in the Jira Connect application together. This grouping allows us to enable our `AllowfIframe` plug for all of these routes at the same time.

```elixir
scope "/jira", MyAppWeb.Jira, as: :jira do
  pipe_through [:browser, :jira]

  live "/", PageLive, :index
  get "/issue/:id", IssueController, :show
end
```

I also leverage Phoenix's [scope](https://hexdocs.pm/phoenix/routing.html#scoped-routes) block to group all routes to be nested under `/jira` and expect all modules to be namespaced with `MyAppWeb.Jira`. These groupings should help keep our codebase easier to manage.

## Cookies

At this point, we can render our application within an `iframe` in Jira (or wherever you set your `frame-ancestors`). However, when you run the application, you may find issues with the user session. In my case,  the frame would continuously reload when trying to a LiveView page.

The Phoenix server tried to log a message hinting at the problem:

```log
[debug] LiveView session was misconfigured
or the user token is outdated.
```

This log message also included suggestions for resolving the issue, but the project generator created the project to follow the suggested practices already.

One of the suggestions for resolving the issues was to make sure to include the CSRF token. When looking at the server logs, I notice the `_csrf_token` was included in the parameters when attempting to connect to the socket, but that it was changing with every page reload and socket reconnect attempt.

After some digging, I eventually found out the problem was the cookie, which is supposed to store information like the CSRF token, was not getting set when loading the `iframe` from within Jira. 

Our cookie was not getting set because the default behavior for cookies is to be [first-party](https://web.dev/samesite-cookies-explained/#what-are-first-party-and-third-party-cookies), meaning they are only accessible on the same domain as the server. Since we are rendering our site in an `iframe` we are attempting to access the cookies for our application from an Atlassian/Jira domain. Working with cookies from a different domain is known as [third-party](https://web.dev/samesite-cookies-explained/#what-are-first-party-and-third-party-cookies) cookies. For security reasons, browsers block this type of cookie by default.

To allow your application to store cookies that can be accessible by a third-party the cookie must have the [`SameSite`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite) property set to `'None'`. When sending cookies to third parties, you must also set the [`Secure`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#Secure) property on the cookie. This indicates that the cookie should only be accessible when the requesting site is using `https` (or is `localhost`). For more information on the `SameSite` options when working with cookies check out [this article](https://web.dev/samesite-cookies-explained/).

### Third Party Cookies with Phoenix

Phoenix has a [`Plug.Session`](https://hexdocs.pm/plug/Plug.Session.html) that is responsible for handling session cookies and stores.  While we added the plug that we created above into the router, this plug is an [Endpoint plug](https://hexdocs.pm/phoenix/plug.html#endpoint-plugs) that Phoenix adds at project generation time. In your project's endpoint module (`lib/my_app_web/endpoint.ex`), you should have a line like:

```elixir
plug Plug.Session, @session_options
```

This file should also include a [module attribute](https://elixir-lang.org/getting-started/module-attributes.html) named `@session_options`. On a recently generated Phoenix application, it should look something like:

```elixir
# The session will be stored in the cookie and signed,
# this means its contents can be read but not tampered with.
# Set :encryption_salt if you would also like to encrypt it.
@session_options [
  store: :cookie,
  key: "_my_app_key",
  signing_salt: "Salt"
]
```

This is where we can set the additional [`Plug.Session` options](https://hexdocs.pm/plug/Plug.Session.html#module-options), including `:same_site` and `:secure`. Our `@session_options` should now look something lke:

```elixir{5-6}
@session_options [
  store: :cookie,
  key: "_my_app_key",
  signing_salt: "Salt"
  same_site: "None",
  secure: true
]
```

## Conclusion

At this point, you should now be able to render your application within an `iframe`. If you copied the `Content-Security-Policy` exactly, you would be limited to self-served and Atlassian `iframe`s, but, hopefully, you will be able to tweak our `AllowIframe` plug to fit your application's needs.



# Thoughts

- Should we include screenshots?
  - Show the iframe not working? Show a hello page?
