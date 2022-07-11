-module(alinkdata).
-export([connect/1]).

-export([start/0, query_from_mysql/2, query_from_redis/2]).

-define(POOL(Type, Name), list_to_atom(lists:concat([Type, Name]))).

start() ->
    start(mysql, application:get_env(alinkdata, mysql, [])),
    start(redis, application:get_env(alinkdata, redis, [])).

query_from_mysql(Pool, SQL) ->
    poolman:transaction(?POOL(mysql, Pool),
        fun(Pid) ->
            mysql:query(Pid, SQL, 30000)
        end).

query_from_redis(Pool, Command) ->
    poolman:transaction(?POOL(redis, Pool),
        fun(Pid) ->
            eredis:q(Pid, Command)
        end).

start(_, []) -> ok;
start(Type, [Opt | Opts]) ->
    Name = proplists:get_value(pool, Opt, default),
    PoolSize = proplists:get_value(pool_size, Opt, 1),
    PoolArgs = [
        {size, PoolSize},
        {auto_size, true},
        {pool_type, random},
        {worker_module, {con, ?MODULE}}
    ],
    WorkerArgs = [Type, Opt],
    Spec = poolman:pool_spec(?POOL(Type, Name), PoolArgs, WorkerArgs),
    {ok, _} = supervisor:start_child(alinkdata_sup, Spec),
    start(Type, Opts).

connect([mysql, Opts]) ->
    mysql:start_link(Opts);
connect([redis, Opts]) ->
    eredis:start_link([{reconnect_sleep, no_reconnect} | Opts]).
