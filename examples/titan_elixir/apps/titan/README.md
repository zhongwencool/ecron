### Ecron Elixir Example

```shell script
$ git clone https://github.com/zhongwencool/ecron.git
$ cd example/titan_elixir
$ mix deps.get
$ iex -s Mix
iex(1)> :ecron.statstictis()
```

* [configure static job task](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir/config/config.exs).
* [stateless cron task](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir/apps/titan/lib/titan/stateless_cron.ex).
* [stateful cron task by :ecron.send_after](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir/apps/titan/lib/titan/stateful_cron/send_after.ex).
* [stateful cron task by :ecron.send_interval](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir/apps/titan/lib/titan/stateful_cron/send_interval.ex).
 
