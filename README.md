# jamesbot

your tweets will be automatically deleted.

### requirement
* Erlang/OTP 19
* rebar3
* wscat(if you need)

### usage
1. rename `config.hrl_sample` to `config.hrl`, and put your keys and secret keys in there.
2. `rebar3 shell` to run.
3. `wscat -c localhost:8080` to connect the app.
4. you can tweet now!
