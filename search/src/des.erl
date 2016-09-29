-module(des).
-compile(export_all).


%% -----------------------------------------------------------------------------
%% 
%% -spec parse_des(List_Des) ->  Des when
%%       List_Des :: [tuple()], 
%%       Des :: [T]
%%       T :: binary()
%% -----------------------------------------------------------------------------
 %% [{<<"p">>,[],
 %%   [<<"This module is used to load all Erlang modules into\n      the system. The start script is also fetched with this low-level\n      loader.">>]},
 %%  {<<"p">>,[],
 %%   [{<<"span">>,[{<<"class">>,<<"code">>}],[<<"erl_prim_loader">>]},
 %%    <<" knows about the environment and how to\n      fetch modules.">>]},
 %%  {<<"p">>,[],
 %%   [<<"Command-line flag ">>,
 %%    {<<"span">>,[{<<"class">>,<<"code">>}],[<<"-loader Loader">>]},
 %%    <<" can be used to\n      choose the method used by ">>,
 %%    {<<"span">>,[{<<"class">>,<<"code">>}],[<<"erl_prim_loader">>]},
 %%    <<". Two\n      ">>,
 %%    {<<"span">>,[{<<"class">>,<<"code">>}],[<<"Loader">>]},
 %%    <<" methods are supported by the Erlang runtime system:\n      ">>,
 %%    {<<"span">>,[{<<"class">>,<<"code">>}],[<<"efile">>]},
 %%    <<" and ">>,
 %%    {<<"span">>,[{<<"class">>,<<"code">>}],[<<"inet">>]},
 %%    <<".">>]}]

parse_des([])->
    [];
parse_des([Tuple|Tail]) ->
    Value = case element(1,Tuple) of
                <<"p">> ->
                    p(Tuple);
                <<"div">> ->
                    div_f(Tuple);
                <<"dl">> ->
                     dl(Tuple)   
                end,
    [Value|parse_des(Tail)].


p(Tup_p)->
    case element(3, Tup_p) of
        [A] when is_binary(A) ->
            btl(A);
        B when is_list(B) ->
            lists:append(loop_list_content_p(B))
    end.
dl(Tup_dl) ->
    case element(3, Tup_dl) of
        [A] when is_binary(A) ->
            btl(A);
        B when is_list(B) ->
            lists:append(loop_list_content_dl(B))
    end.
loop_list_content_dl([]) ->
    [];
loop_list_content_dl([Head|Tail]) when element(1,Head) == <<"dt">> ->
    [btl(dt(Head))|loop_list_content_dl(Tail)];
loop_list_content_dl([Head|Tail]) when element(1,Head) == <<"dd">> ->
    [btl(dd(Head))|loop_list_content_dl(Tail)].
dt(Tup_dt)->
    case element(3,Tup_dt) of
        [{<<"strong">>,_,[Content]}] when element(1,Content) == <<"span">> ->
            span(Content);
        _ ->
            undedined
    end.
dd(Tup_dd)->
    case element(3,Tup_dd) of
        [A] when is_binary(A) ->
            btl(A);
        B when is_list(B) ->
            lists:append(loop_list_content_dd(B))
    end.
loop_list_content_dd([])->
    [];
loop_list_content_dd([Head|Tail]) when is_binary(Head) ->
    [btl(Head)|loop_list_content_dd(Tail)];
loop_list_content_dd([Head|Tail]) when element(1,Head) == <<"span">> ->
    [btl(span(Head))|loop_list_content_dd(Tail)].


loop_list_content_p([])->
    [];
loop_list_content_p([Head|Tail]) when is_binary(Head) ->
    [btl(Head)|loop_list_content_p(Tail)];
loop_list_content_p([Head|Tail]) when element(1,Head) == <<"span">> ->
    [btl(span(Head))|loop_list_content_p(Tail)].

span(Tup_span)->
    case element(3,Tup_span) of
        [A] when is_binary(A) ->
            A;
        [{<<"a">>,_,[B]}] when is_binary(B)->
            B;
        [{<<"a">>,_,[Content]}] ->
            span(Content);
        _ ->
            undefined
    end.
div_f(Tup_div) ->
    case element(3,Tup_div) of
        [A] when is_binary(A) ->
            A;
        [{<<"pre">>,_,[B]}] when is_binary(B) ->
            B;
        [{<<"pre">>,_,[Content]}] ->
            div_f(Content);
        _ ->
            undefined
    end.

clean(Arg)->    
    re:replace(Arg, "\n", "~n", [global,{return,list}]).
loop_print([])->
    ok;
loop_print([Head|Tail]) ->
    io:format("~s~n", [Head]),
    loop_print(Tail).
btl(Arg)->
    binary:bin_to_list(Arg).
ltb(Arg)->
    binary:list_to_bin(Arg).












