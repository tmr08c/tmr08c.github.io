---
title: 'Running a Phoenix App in an iframe'
date: '2020-11-14T05:31:13.265Z'
categories: ['elixir', 'phoenix']
---

For a side project, I am working on building a [Jira Connect application](https://developer.atlassian.com/cloud/jira/platform/#atlassian-connect) using [Elixir](https://elixir-lang.org/) and [Phoenix](https://www.phoenixframework.org/). When building a Connect app, you run your own server, but your UI is rendered within Jira as though it is a part of Jira itself. This in-Jira UI rendering is done via an [`iframe`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe).

To get this to work securely with my Phoenix application, I needed to:

1. Update the `Content-Security-Policy` response headers to allow some of the application's pages to be embedded in an `iframe`.
2. Update the `SameSite` settings for the session cookie to allow the cookies to be used by a third-party.

In this post, I will walk through how I set that up for this project.

## `Content-Security-Policy`

The [`Content-Security-Policy` headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy) provide a mechanism for the server to tell the browser what content is safe to load. A common use case for this is to indicate which JavaScript should actually come from the server, blocking the rest and preventing [XSS attacks](https://developer.mozilla.org/en-US/docs/Glossary/Cross-site_scripting).

In this case, we want to specify where it is okay to render our `iframe`. For this, we use the [`frame-ancestors`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors) directive. If you are familiar with the header, [`X-Frame-Option`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options) setting `Content-Security-Policy` with the `frame-ancestors` directive is the [recommended replacement](https://www.w3.org/TR/CSP2/#frame-ancestors-and-frame-options) for this header.

The `fame-ancestors` directive expects a list of sources (e.g., URLs). For the purposes of the Connect app I was building, I wanted to allow the `iframe` to be rendered in any cloud instance of Jira.

To work with any cloud instance of Jira, I was able to leverage the ability to use wildcard matchers to match any subdomain of `atlassian.net`. The syntax for ths is `https://*.atlassian.net`.

In addition to allowing the content to be rendered in an `iframe` on any Atlassian subdomain, I also decided to include the `'self'` source. This allows `iframe`s to be rendered when the `iframe`'s `src` matches the origin the user is connected to (when the page is requesting `iframe` content from its `self`). For now, it is included to allow for manual testing, but, depending on your needs and testing strategy, you may be able to remove it.

With these two needs in mind, my goal was to end up with a `Content-Security-Policy` header that matched:

```html
Content-Security-Policy: frame-ancestors 'self' https://*.atlassian.net
```

### Allow `iframe` Plug

To actually add this response header, we are going to leverage Phoenix's use of [Plug](https://hexdocs.pm/phoenix/plug.html). Plug is a library that allows you to interact with HTTP requests and responses in a modular manner. The core of Phoenix's HTTP lifecycle is handled through plugs.

We will create a [module plug](https://hexdocs.pm/phoenix/plug.html#module-plugs) like the following.

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

Now that we have a plug, we want to add it to our [router](https://hexdocs.pm/phoenix/plug.html#controller-plugs) so it will actually be used.

In order to use a plug in the `router`, it must be [included in a pipeline](https://hexdocs.pm/phoenix/plug.html#router-plugs). Since we are building a Connect application, I decided to make a `jira` `pipeline` that would be used for requests that are expected to be rendered within Jira.

```elixir
# This pipeline should be used for routes that are expected to be requested as
# a part of the Jira Connect application
pipeline :jira do
  plug MyAppWeb.Plugs.AllowIframe
end
```

I didn't add this directly to the existing `browser` `pipeline` because I anticipated the need to have some routes that would not be required to render within the Jira UI (and therefore not in an `iframe`). While you needs will be different, please remember the ability to render `iframe`s is limited for security purposes. I would suggest trying to limit the ability to display your application in an `iframe` as much as possible and allow access on an as-needed basis instead of defaulting to making it available.

With our `pipeline` in place, I can group routes that will be rendered in the Jira Connect application together to make them easier to find and to also have them all be able to be rendered in an `iframe`:

```elixir
scope "/jira", MyAppWeb.Jira, as: :jira do
  pipe_through [:browser, :jira]

  live "/", PageLive, :index
  get "/issue/:id", IssueController, :show
end
```

I also leverage Phoenix's [scope](https://hexdocs.pm/phoenix/routing.html#scoped-routes) block to group all routes to be nested under `/jira` and expect all modules to be namespaced with `MyAppWeb.Jira`.

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

This doesn't test the any of the app's routes actually _use_ this plug, but does provide a a sanity check that the plug updats our response headers to match what we were hoping for. For now, I am happy enough with this. I do plan to explore whether I could add the additional ease-of-mind by making sure routes I expect to be renderable in an `iframe` will work as expected.

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

Phoenix has a [`Plug.Session`](https://hexdocs.pm/plug/Plug.Session.html) that is responsible for handling session cookies and stores.  While we added the plug that we created above into the router, this plug is an [Endpoint plug](https://hexdocs.pm/phoenix/plug.html#endpoint-plugs) that Phoenix adds at project generation time. In your project's endpoint module (`lib/my_app_web/endpoint.ex`) you should have a line like:

```elixir
plug Plug.Session, @session_options
```

You should also have a [module attribute](https://elixir-lang.org/getting-started/module-attributes.html) in the file names `@session_options`. On a recently generated Phoenix application, it should look something like:

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

This is where we can set the additional [`Plug.Session` options](https://hexdocs.pm/plug/Plug.Session.html#module-options), including `:same_site`. 

In addition to setting the `same_site` key, we will also need to set the `secure` key. As noted above, this is a requirement for (modern) browsers to accept third-party cookies.

Our `@session_options` should now look something lke:

```elixir{5-6}
@session_options [
  store: :cookie,
  key: "_my_app_key",
  signing_salt: "Salt"
  secure: true,
  same_site: "None"
]
```

## Conclusion

At this point, you should not be able to render your applicaton within an `iframe`. If you followed exactly, you would be limited to self-served and Atlassian `iframe`s, but, hopefully, you will be able to tweak our `AllowIframe` plug to fit your application's needs. 


# Thoughts

- Should we include screenshots?
  - Show the iframe not working? Show a hello page?
