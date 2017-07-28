# DalmatinerDB PostgreSQL Indexer Auto Publisher

An auto populator for DalmatinerDB PostgreSQL indexer.

This software shall automatically poll for the ddb metrics
and populate the PostgreSQL server with metadata appropriate
for DalmatinerDB Frontend to use. The actor will NOT delete
old entries in case of expiry of metrics.

There is one actor per bucket, which will observe it for
changes.

> [ddb_proxy](https://github.com/dalmatinerdb/ddb_proxy) is used
> as the bootstrap for this project.

Build
-----

```bash
$ rebar3 compile
```

Release
-------

```bash
$ rebar3 release
```

Todo
----

1. Automatically discover non-existent keys in ddb buckets and remove
   from the PostgreSQL index table.

