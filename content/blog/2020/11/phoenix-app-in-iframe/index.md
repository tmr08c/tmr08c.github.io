---
title: 'Running a Phoenix App in an iframe'
date: '2020-11-14T05:31:13.265Z'
categories: ['elixir', 'phoenix']
---

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
& https://developer.atlassian.com/cloud/jira/platform/#atlassian-connect
& https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy/frame-ancestors
