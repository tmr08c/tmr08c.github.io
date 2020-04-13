## Intro
  * want to learn about `conccurent-ruby`
  * chose `Async` because loosely based on Erlang's `gen_server`
    * there is mor feature rich Actor (confirm: I think it's still under active development)
## Understanding how it works
  * Making sample class

    * Copied exapmle from docs, but added `sleep` to see the impact 

      ```ruby
      class Hello
        include Concurrent::Async

        # @return [Concurrent::IVar]
        def hello(name)
          sleep(3)
          puts "Hello, #{name}"
        end
      end
      ```
  * Made CLI for interacting
  * Basics - call with `async` call with `await` 
  * When are threads made
    * adding thread counter to CLI
    * trying with `.new`
    * `.new` and calling method (works), uses same thread for same variable because it's copying the actor model and queue 
    * can make new instances and call method everytime to make it work
    * lazy thread creation - eventually get to https://github.com/ruby-concurrency/concurrent-ruby/blob/7dc6eb04142f008ffa79a59c125669c6fcbb85a8/lib/concurrent-ruby/concurrent/executor/ruby_executor_service.rb#L17-L25
## conclusion
  * just scratching surface of libaray 
  * enjoying sleep learning - try something, poke around code, no rush to build
  * link to code

# TODO
- [ ] add commands to test.rb that re-uses variable and one that creates new
- [ ] create README in demo app
- [ ] create repo for demo app
