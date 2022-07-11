-module(alinkiot_user).
-include("alinkdata.hrl").
%% API
-export([start/0, login/3, logout/3, get_info/2, get_routers/2]).

-spec start() -> ok.
start() ->
    ehttpd_hook:add('user.login', {?MODULE, login}),
    ehttpd_hook:add('user.logout', {?MODULE, logout}),
    ehttpd_hook:add('user.get_info', {?MODULE, get_info}),
    ehttpd_hook:add('user.get_routers', {?MODULE, get_routers}),
    ok.


login(UserName, Password, Result) ->
    case query_user(UserName, Password) of
        {ok, UserInfo} ->
            Ts = os:system_time(second),
            Token = ehttpd_utils:md5(lists:concat([binary_to_list(UserName), Ts])),
            TTL = ehttpd_server:get_env(default, expire, 1800),
            ehttpd_auth:put_session(Token, UserInfo, TTL),
            {ok, Result#{
                code => 200,
                token => Token
            }};
        {error, Reason} ->
            {stop, Result#{
                code => 404,
                msg => Reason
            }}
    end.


get_info(#{ user := UserInfo }, Result) ->
    Result1 = maps:merge(UserInfo, Result),
    {ok, Result1#{
        code => 200,
        msg => <<"success">>
    }}.

get_routers(_Context, Result) ->
    Table = <<?PREFIX/binary, "menu">>,
    Query = #{ <<"pageSize">> => 1000 },
    case alinkdata_mysql:query(default, Table, Query) of
        {ok, Rows} ->
            {ok, Result#{
                code => 200,
                data => create_route(Rows, #{})
            }};
        {error, Reason} ->
            Msg = list_to_binary(io_lib:format("~p", [Reason])),
            {stop, Result#{
                code => 500,
                msg => Msg
            }}
    end.

logout(_Args, Context, Result) ->
    case maps:get(token, Context, undefined) of
        undefined ->
            {ok, Result};
        Token ->
            ehttpd_auth:delete_session(Token),
            {ok, Result}
    end.


query_user(UserName, _Password) ->
    {ok, #{
        <<"permissions">> => [<<"*:*:*">>],
        <<"roles">> => [<<"admin">>],
        <<"user">> => #{
            <<"username">> => UserName,
            <<"nickName">> => <<"zwx">>,
            <<"email">> => <<"5422@qq.com">>
        }
    }}.

create_route([], Acc) ->
    Menus = maps:get(0, Acc, []),
    [add_children(Menu, Acc) || Menu <- Menus];
create_route([#{<<"parent_id">> := ParentId } = Row | Rows], Acc) ->
    case filter_route(Row) of
        false ->
            create_route(Rows, Acc);
        true ->
            Menus = maps:get(ParentId, Acc, []),
            create_route(Rows, Acc#{ParentId => Menus ++ [format_route(Row)]})
    end.

add_children(#{<<"menu_id">> := Id} = Menu, Acc) ->
    case maps:get(Id, Acc, []) of
        [] ->
            Menu;
        Children ->
            Menu#{
                <<"alwaysShow">> => true,
                <<"redirect">> => <<"noRedirect">>,
                <<"children">> => [add_children(Child, Acc) || Child <- Children]
            }
    end.


filter_route(#{ <<"menu_type">> := <<"F">>}) ->
    false;
filter_route(#{<<"path">> := <<"#">>}) ->
    false;
filter_route(#{<<"path">> := <<>>}) ->
    false;
filter_route(_) ->
    true.

format_route(#{
    <<"menu_id">> := Id,
    <<"menu_name">> := Title,
    <<"path">> := Path,
    <<"visible">> := Visible,
    <<"is_cache">> := IsCache,
    <<"component">> := Component,
    <<"parent_id">> := ParentId,
    <<"icon">> := Icon,
    <<"is_frame">> := IsFrame
}) ->
    #{
        <<"menu_id">> => Id,
        <<"name">> => get_name(Path),
        <<"path">> => get_path(ParentId, Path),
        <<"hidden">> => Visible =/= <<"0">>,
        <<"component">> => get_component(Component),
        <<"meta">> => #{
            <<"title">> => Title,
            <<"icon">> => Icon,
            <<"noCache">> => IsCache =/= 0,
            <<"link">> => get_link(IsFrame, Path)
        }
    }.

get_component(null) ->
    <<"Layout">>;
get_component(<<>>) ->
    <<"ParentView">>;
get_component(Component) ->
    Component.

get_path(_, <<"http://", _/binary>> = Path) -> Path;
get_path(0, Path) -> <<"/", Path/binary>>;
get_path(_, Path) -> Path.

get_name(<<>>) -> <<>>;
get_name(<<P:1/bytes, Path/binary>>) ->
    P1 = list_to_binary(string:to_upper(binary_to_list(P))),
    <<P1/binary, Path/binary>>.

get_link(0, Path) -> Path;
get_link(_, _) -> null.
