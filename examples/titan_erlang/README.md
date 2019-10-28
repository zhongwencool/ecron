### Ecron Erlang Example

```shell script
$ git clone https://github.com/zhongwencool/ecron.git
$ cd example/titan_erlang
$ rebar3 get-deps
$ rebar3 shell
erl(1)> ecron:statstictis().
```

* [configure static job task](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang/config/sys.config).
* [stateless cron task](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang/apps/titan/src/stateless_cron.erl).
* [stateful cron task by ecron:send_after](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang/apps/titan/src/stateful_cron_by_send_after.erl).
* [stateful cron task by ecron:send_interval](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang/apps/titan/src/stateful_cron_by_send_interval.erl).
