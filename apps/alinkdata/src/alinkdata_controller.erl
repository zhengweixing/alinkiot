-module(alinkdata_controller).
-include("alinkdata.hrl").
-behavior(ehttpd_rest).
-ehttpd_rest(default).
-export([swagger/1, handle/4, load_from_file/0, load_from_mysql/0]).


swagger(default) ->
    Path = code:priv_dir(alinkdata),
    Global = filename:join([Path, "swagger_global.json"]),
    {ok, Data} = file:read_file(Global),
    AccSchema = jiffy:decode(Data, [return_maps]),
    TplPath = filename:join([Path, "swagger_curd.json"]),
    case erlydtl:compile({file, TplPath}, render, [{out_dir, false}]) of
        {ok, Render} ->
            {ok, Tables} = load_from_mysql(),
            lists:foldl(
                fun(Table, Acc) ->
                    [create_swagger(Table, Render) | Acc]
                end, [AccSchema], Tables);
        error ->
            {error, dtl_compile_error}
    end.


-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map(), Req :: ehttpd_req:req()}.
handle(_OperationID, Args, Context, Req) ->
    do_query(Context, Args, Req).

do_query(#{
    extend := #{
        <<"action">> := <<"SCHEMA">>
    }
} = _Context, _Args, Req) ->
    Format =
        fun(#{
            <<"tableName">> := TableName,
            <<"priFields">> := PriFields,
            <<"fields">> := Fields
        } = Info) ->
            NewFields = [Field#{
                <<"type">> => alinkdata_formater:format_type(Type)
            } || #{<<"type">> := Type} = Field <- Fields],
            ID = <<TableName/binary, "_id">>,
            Id = case lists:member(ID, PriFields) of
                     true -> ID;
                     false -> lists:last(PriFields)
                 end,
            Info#{
                <<"id">> => Id,
                <<"fields">> => NewFields
            }
        end,
    case alinkdata_mysql:get_schema(default, Format) of
        {error, Reason} ->
            Msg = list_to_binary(io_lib:format("~p", [Reason])),
            response(500, #{code => 500, msg => Msg}, Req);
        {ok, Tables} ->
            response(200, #{ total => length(Tables), rows => Tables }, Req)
    end;

do_query(#{
    extend := #{
        <<"table">> := Table,
        <<"action">> := <<"QUERY">>
    }
} = _Context, Args, Req) ->
    io:format("~p ~p~n", [Table, Args]),
    Format =
        fun(Field, Value, Acc) ->
            alinkdata_formater:format_field(Table, Field, Value, Acc)
        end,
    Data = #{ code => 200 },
    case alinkdata_mysql:query(default, get_table(Table), Args, Format) of
        {ok, Rows} ->
            response(200, Data#{data => Rows}, Req);
        {ok, Count, Rows} ->
            response(200, Data#{total => Count, rows => Rows}, Req);
        {error, Reason} ->
            Msg = list_to_binary(io_lib:format("~p", [Reason])),
            response(500, #{code => 500, msg => Msg}, Req)
    end;

do_query(#{
    extend := #{
        <<"table">> := Table,
        <<"action">> := <<"GET">>
    }
} = _Context, Args, Req) ->
    Format =
        fun(Field, Value, Acc) ->
            alinkdata_formater:format_field(Table, Field, Value, Acc)
        end,
    case alinkdata_mysql:query(default, get_table(Table), Args#{ <<"pageSize">> => 1 }, Format) of
        {ok, []} ->
            Response = #{code => 200},
            response(200, Response, Req);
        {ok, [Row]} ->
            Data = #{code => 200},
            Response = Data#{data => Row},
            response(200, Response, Req);
        {error, Reason} ->
            Msg = list_to_binary(io_lib:format("~p", [Reason])),
            response(500, #{code => 500, msg => Msg}, Req)
    end;

do_query(#{
    extend := #{
        <<"action">> := <<"TREE">>,
        <<"table">> := <<"dept">> = Table
    }
} = _Context, Args, Req) ->
    Format =
        fun(Field, Value, Acc) ->
            alinkdata_formater:format_field(Table, Field, Value, Acc)
        end,
    Data = #{ code => 200 },
    case alinkdata_mysql:query(default, get_table(Table), Args, Format) of
        {ok, Rows} ->
            Tree = alinkdata_utils:create_tree(Rows, <<"pid">>, <<"id">>, fun format_dept/1),
            response(200, Data#{data => Tree}, Req);
        {error, Reason} ->
            Msg = list_to_binary(io_lib:format("~p", [Reason])),
            response(500, #{code => 500, msg => Msg}, Req)
    end;

do_query(#{
    extend := #{
        <<"table">> := _Table,
        <<"action">> := <<"ADD">>
    }
} = _Context, _Args, Req) ->
    response(200, #{}, Req);

do_query(#{
    extend := #{
        <<"table">> := _Table,
        <<"action">> := <<"PUT">>
    }
} = _Context, _Args, Req) ->
    response(200, #{}, Req);

do_query(#{
    extend := #{
        <<"table">> := Table,
        <<"action">> := <<"DELETE">>
    }
} = _Context, Args, Req) ->
    case alinkdata_mysql:delete(default, get_table(Table), Args) of
        ok ->
            response(200, #{}, Req);
        {error, Reason} ->
            response(400, #{ msg => Reason }, Req)
    end;

do_query(#{
    extend := #{
        <<"table">> := _Table,
        <<"action">> := <<"IMPORT">>
    }
} = _Context, _Args, Req) ->
    response(200, #{}, Req);

do_query(#{
    extend := #{
        <<"table">> := _Table,
        <<"action">> := <<"EXPORT">>
    }
} = _Context, _Args, Req) ->
    response(200, #{}, Req).

load_from_file() ->
    Path = code:priv_dir(alinkdata),
    {ok, Data} = file:read_file(filename:join([Path, "tables.json"])),
    Tables = jiffy:decode(Data, [return_maps]),
    {ok, Tables}.

load_from_mysql() ->
    Format =
        fun(#{
            <<"tableName">> := TableName,
            <<"priFields">> := PriFields,
            <<"fields">> := Fields
        } = Info) ->
            NewFields = [Field#{
                <<"type">> => alinkdata_formater:format_type(Type)
            } || #{<<"type">> := Type} = Field <- Fields],
            ID = <<TableName/binary, "_id">>,
            Id = case lists:member(ID, PriFields) of
                     true -> ID;
                     false -> lists:last(PriFields)
                 end,
            Info#{
                <<"id">> => Id,
                <<"tableName">> => binary:replace(TableName, ?PREFIX, <<>>),
                <<"fields">> => NewFields
            }
        end,
    case alinkdata_mysql:get_schema(default, Format) of
        {error, Reason} ->
            {error, Reason};
        {ok, Tables} ->
            {ok, Tables}
    end.

response(Status, Data, Req) ->
    Header = #{},
    {Status, Header, Data, Req}.

create_swagger(#{
    <<"id">> := Id,
    <<"tableName">> := TableName,
    <<"fields">> := Fields
}, Render) ->
    Fields1 =
        lists:foldl(
            fun(Field, Acc) ->
                [maps:fold(
                    fun(Key, Value, Acc1) ->
                        [{binary_to_atom(Key), Value} | Acc1]
                    end, [], Field) | Acc]
            end, [], Fields),
    Vals = [{id, Id}, {tableName, TableName}, {fields, Fields1}],
    {ok, IoList} = Render:render(Vals),
    jiffy:decode(unicode:characters_to_binary(IoList), [return_maps]).

format_dept(#{
    <<"deptId">> := Id,
    <<"deptName">> := Name,
    <<"parentId">> := ParentId
}) ->
    #{
        <<"label">> => Name,
        <<"id">> => Id,
        <<"pid">> => ParentId
    }.

get_table(<<"gen_table">>) -> <<"gen_table">>;
get_table(<<"gen_table_column">>) -> <<"gen_table_column">>;
get_table(Table) -> <<?PREFIX/binary, Table/binary>>.
