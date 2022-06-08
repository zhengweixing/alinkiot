%%%-------------------------------------------------------------------
%% @doc alinkiot public API
%% @end
%%%-------------------------------------------------------------------

-module(alinkiot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    alinkiot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
