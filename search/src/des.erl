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
		<<"ul">> ->
		    ul(Tuple);
                <<"dl">> ->
		    dl(Tuple)   
                end,
    [Value | parse_des(Tail)].


p(Tup_p)->
    case element(3, Tup_p) of
        [A] when is_binary(A) ->
            btl(A);
	[C] when is_tuple(C) andalso element(1,C) == <<"p">> ->
	    btl(p(C));
        B = [_|_] ->
            lists:append(loop_list_content_p(B))
    end.
dl(Tup_dl) ->
    case element(3, Tup_dl) of
        [A] when is_binary(A) ->
            btl(A);
        B when is_list(B) ->
            lists:append(loop_list_content_dl(B))
    end.
ul(Tup_ul) ->
    case element(3, Tup_ul) of
	A = [_|_] ->
	    lists:append(loop_list_content_ul(A));
	_ ->
	    []
    end.
li(Tup_li) ->
    case element(3, Tup_li) of
	[A] when is_tuple(A) andalso element(1,A) == <<"p">> ->
	    p(A);
	_ ->
	    []
    end.

dt(Tup_dt)->
    case element(3,Tup_dt) of
	[A] when is_tuple(A) andalso element(1,A) == <<"strong">> ->
	    strong(A);
        _ ->
            []
    end.
dd(Tup_dd)->
    case element(3,Tup_dd) of
        [A] when is_binary(A) ->
            btl(A);
	[B] when is_tuple(B) andalso element(1,B) == <<"p">> ->
	    p(B);
        C = [_|_] ->
            lists:append(loop_list_content_dd(C));
	[] ->
	    []
    end.

loop_list_content_ul([]) ->
    [];
loop_list_content_ul([Head|Tail]) when element(1,Head) == <<"li">> ->
    [li(Head)|loop_list_content_ul(Tail)].

loop_list_content_dl([]) ->
    [];
loop_list_content_dl([Head|Tail]) when element(1,Head) == <<"dt">> ->
    [btl(dt(Head))|loop_list_content_dl(Tail)];
loop_list_content_dl([Head|Tail]) when element(1,Head) == <<"dd">> ->
    [btl(dd(Head))|loop_list_content_dl(Tail)].

loop_list_content_dd([])->
    [];
loop_list_content_dd([Head|Tail]) when is_binary(Head) ->
    [btl(Head)|loop_list_content_dd(Tail)];
loop_list_content_dd([Head|Tail]) when element(1, Head) == <<"p">> ->
    [p(Head)|loop_list_content_dd(Tail)];
loop_list_content_dd([Head|Tail]) when element(1, Head) == <<"dl">> ->
    [dl(Head)|loop_list_content_dd(Tail)];
loop_list_content_dd([Head|Tail]) when element(1, Head) == <<"div">> ->
    [div_f(Head)|loop_list_content_dd(Tail)];
loop_list_content_dd([Head|Tail]) when element(1, Head) == <<"span">> ->
    [btl(span(Head))|loop_list_content_dd(Tail)].


loop_list_content_p([])->
    [];
loop_list_content_p([Head|Tail]) when is_binary(Head) ->
    [btl(Head)|loop_list_content_p(Tail)];
loop_list_content_p([Head|Tail]) when element(1,Head) == <<"strong">> ->
    [btl(strong(Head))|loop_list_content_p(Tail)];
loop_list_content_p([Head|Tail]) when element(1,Head) == <<"a">> ->
    [btl(a(Head))|loop_list_content_p(Tail)];
loop_list_content_p([Head|Tail]) when element(1,Head) == <<"p">> ->
    [btl(p(Head))|loop_list_content_p(Tail)];
loop_list_content_p([Head|Tail]) when element(1,Head) == <<"span">> ->
    [btl(span(Head))|loop_list_content_p(Tail)].

loop_list_content_strong([])->
    [];
loop_list_content_strong([Head|Tail]) when is_binary(Head) ->
    [btl(Head)|loop_list_content_strong(Tail)];
loop_list_content_strong([Head|Tail]) when element(1,Head) == <<"span">> ->
    [btl(span(Head))|loop_list_content_strong(Tail)].

loop_list_content_div([])->
    [];
loop_list_content_div([Head|Tail]) when element(1,Head) == <<"div">> ->
    [div_f(Head)|loop_list_content_div(Tail)].

strong(Tup_strong)->
    case element(3,Tup_strong) of
        [A] when is_binary(A) ->
            A;
	[B] when is_tuple(B) andalso element(1, B) == <<"span">> ->
	    span(B);
        C = [_|_] ->
           lists:append(loop_list_content_strong(C));
        _ ->
           []
    end.
    
span(Tup_span)->
    case element(3,Tup_span) of
        [A] when is_binary(A) ->
            A;
        [{<<"a">>,_,[B]}] when is_binary(B)->
            B;
        [{<<"a">>,_,[Content]}] ->
            span(Content);
        _ ->
            []
    end.
pre(Tup_pre)->
    case element(3,Tup_pre) of
	[A] when is_binary(A) ->
	    btl(A);
	_ ->
	    []
    end.
a(Tup_a)->
    case element(3,Tup_a) of
	[]  ->
	    [];
	_ ->
	    []
    end.
div_f(Tup_div) ->
    case element(3, Tup_div) of
	[B] when is_binary(B) ->
	    btl(B);
	[C] when is_tuple(C) andalso element(1,C) == <<"pre">> ->
	    pre(C);
	[D] when is_tuple(D) andalso element(1,D) == <<"p">> ->
	    btl(p(D));
	A = [_|_] ->
	    lists:append(loop_list_content_div(A));
        _ ->
            []
    end.

clean(Arg)->    
    re:replace(Arg, "\n", "~n", [global,{return,list}]).
loop_print([])->
    ok;
loop_print([Head|Tail]) ->
    io:format("~s~n", [Head]),
    loop_print(Tail).
btl(Arg)->
    case is_binary(Arg) of
	true ->
	    binary:bin_to_list(Arg);
	_ ->
	    Arg
    end.
ltb(Arg)->
    binary:list_to_bin(Arg).





