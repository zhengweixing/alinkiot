-module(alinkdata_mysql).

%% API
-export([query/3, query/4, delete/3, update/3, create/3, get_schema/2]).


query(Pool, Table, Query) ->
    Format = fun(Key, Value, Acc) -> Acc#{ Key => Value } end,
    query(Pool, Table, Query, Format).


query(Pool, Table, Query, Format) ->
    PageSize = maps:get(<<"pageSize">>, Query, 100),
    Keys = maps:get(<<"keys">>, Query, undefined),
    Order = maps:get(<<"order">>, Query, undefined),
    Where = maps:without([<<"pageNum">>, <<"pageSize">>, <<"keys">>, <<"order">>], Query),
    SQL1 = format_select(Table, Keys),
    SQL2 = format_where(Where),
    SQL4 = format_order(Order),
    PageNum = maps:get(<<"pageNum">>, Query, undefined),
    SQL3 = format_limit(PageNum, PageSize),
    SQL = case PageNum of
              undefined ->
                  <<SQL1/binary, SQL2/binary, SQL3/binary, SQL4/binary, ";">>;
              _ ->
                  SQL5 = <<"SELECT COUNT(*) FROM ", Table/binary, SQL2/binary, ";">>,
                  <<SQL5/binary, SQL1/binary, SQL2/binary, SQL3/binary, SQL4/binary, ";">>
          end,
    case alinkdata:query_from_mysql(Pool, binary_to_list(SQL)) of
        {ok, [{_, [[Count]]}, {Fields, Rows}]} ->
            {ok, Count, format_result(Fields, Rows, Format)};
        {ok, Fields, Rows} ->
            {ok, format_result(Fields, Rows, Format)};
        {error, Reason} ->
            logger:error("[SQL ERROR]~p,~p", [SQL, Reason]),
            {error, Reason}
    end.


delete(Pool, Table, Where) ->
    SQL1 = format_where(Where),
    SQL = <<"DELETE FROM ", Table/binary, SQL1/binary, ";">>,
    alinkdata:query_from_mysql(Pool, binary_to_list(SQL)).

update(Pool, Table, Args) ->
    SQL1 = alinkdata_utils:join(",", format_update(Args)),
    SQL = lists:concat(["UPDATE ", binary_to_list(Table), " SET ", SQL1, ";"]),
    alinkdata:query_from_mysql(Pool, SQL).

create(Pool, Table, Args) ->
    {Fields0, Values0} =
        maps:fold(
            fun(Field, Value, {Fields, Values}) ->
                {Field1, Value1} = format_create(Field, Value),
                {[Field1 | Fields], [Value1 | Values]}
            end, {[], []}, Args),
    Fields = alinkdata_utils:join(",", Fields0),
    Values = alinkdata_utils:join(",", Values0),
    SQL = lists:concat(["INSERT INTO ", binary_to_list(Table), "(", Fields, ") VALUES (", Values, ");"]),
    alinkdata:query_from_mysql(Pool, SQL).


get_schema(Pool, Format) ->
    case alinkdata:query_from_mysql(Pool, "show tables;") of
        {ok, _, Tables} ->
            SQL = list_to_binary([binary_to_list(<<"desc ", Table/binary, ";">>) || [Table] <- Tables]),
            case alinkdata:query_from_mysql(Pool, SQL) of
                {error, Reason} ->
                    {error, Reason};
                {ok, DataSet} ->
                    {ok, get_schema(Tables, DataSet, Format, [])}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_schema([], [], _, Acc) -> Acc;
get_schema([[TableName] | Tables], [{_, Fields} | DataSet], Format, Acc) ->
    {PriFields, NewFields} =
        lists:foldl(
            fun(Field, {Acc1, Acc2}) ->
                Field1 = create_field(Field),
                case Field1 of
                    #{<<"required">> := true, <<"name">> := FieldName} ->
                        {[FieldName | Acc1], [Field1 | Acc2]};
                    _ ->
                        {Acc1, [Field1 | Acc2]}
                end
            end, {[], []}, Fields),
    Info = #{
        <<"tableName">> => TableName,
        <<"fields">> => NewFields,
        <<"priFields">> => PriFields
    },
    get_schema(Tables, DataSet, Format, [Format(Info) | Acc]).

create_field([FieldName, Type, IsNull, PRI, Default, AutoIncrement]) ->
    #{
        <<"name">> => FieldName,
        <<"type">> => Type,
        <<"required">> => PRI == <<"PRI">>,
        <<"isnull">> => IsNull == <<"YES">>,
        <<"default">> => Default,
        <<"auto_increment">> => AutoIncrement
    }.


format_create(Field, Value) when is_binary(Value) ->
    {Field, <<"'", Value/binary, "'">>};
format_create(Field, Value) when is_integer(Value) ->
    {Field, integer_to_binary(Value)};
format_create(Field, Value) ->
    {Field, Value}.


format_update(Args) ->
    maps:fold(
        fun(Field, Value, Acc) ->
              [format_update(Field, Value)|Acc]
        end, [], Args).

format_update(Field, Value) when is_binary(Value) ->
    <<Field/binary, "='", Value/binary, "'">>;
format_update(Field, Value) when is_integer(Value) ->
    Value1 = integer_to_binary(Value),
    <<Field/binary, "=", Value1/binary>>;
format_update(Field, Value) ->
    list_to_binary(lists:concat([binary_to_list(Field), "=", Value])).


format_select(Table, Keys) when Keys == <<>>; Keys == undefined ->
    <<"SELECT * FROM ", Table/binary>>;
format_select(Table, Keys) ->
    <<"SELECT ", Keys/binary, " FROM ", Table/binary>>.

format_where(Where0) ->
    Str = maps:get(<<"where">>, Where0, undefined),
    Where = maps:remove(<<"where">>, Where0),
    Where1 =
        case Str == undefined of
            true ->
                Where;
            false ->
                maps:merge(Where, jiffy:decode(Str, [return_maps]))
        end,
    List =
        maps:fold(
            fun(Key, Value, Acc) ->
                [format_where_value(Key, Value) | Acc]
            end, [], Where1),
    case alinkdata_utils:join(" and ", List) of
        "" -> <<>>;
        S -> list_to_binary(" WHERE " ++ S)
    end.

format_where_value(Key, Value) when is_binary(Value) ->
    <<Key/binary, "='", Value/binary, "'">>;
format_where_value(Key, Value) when is_integer(Value) ->
    V = integer_to_binary(Value),
    <<Key/binary, "=", V/binary, "">>.

format_limit(PageNum, undefined) ->
    format_limit(PageNum, 100);
format_limit(undefined, PageSize) ->
    list_to_binary(lists:concat([" LIMIT ", PageSize]));
format_limit(PageNum, PageSize) ->
    list_to_binary(lists:concat([" LIMIT ", (PageNum - 1) * PageSize, ",", PageSize])).

format_order(undefined) ->
    <<>>;
format_order(Order) ->
    <<"ORDER BY ", Order/binary>>.

format_result(Fields, Rows, Format) ->
    format_result(Fields, Rows, Format, []).

format_result(_, [], _, Acc) -> Acc;
format_result(Fields, [Row | Rows], Format, Acc) ->
    Row1 = format_row(Fields, Row, Format, #{}),
    format_result(Fields, Rows, Format, lists:append(Acc, [Row1])).

format_row([], [], _, Acc) -> Acc;
format_row([Field | Fields], [Value | Values], Format, Acc) ->
    Acc1 = format_value(Field, Value, Format, Acc),
    format_row(Fields, Values, Format, Acc1).

format_value(Field, {{Y, N, D}, {H, M, S}}, Format, Acc) ->
    Time = list_to_binary((lists:concat([Y, "-", N, "-", D, " ", H, ":", M, ":", S]))),
    Format(Field, Time, Acc);
format_value(Field, Value, Format, Acc) ->
    Format(Field, Value, Acc).


