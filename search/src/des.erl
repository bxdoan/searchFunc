-module(des).
-compile(export_all).


%% -----------------------------------------------------------------------------
%% 
%% -spec parse_des(List_Des) ->  Des when
%%       List_Des :: [tuple()], 
%%       Des :: [T]
%%       T :: binary()
%% -----------------------------------------------------------------------------
parse(Arg)->
    lists:append(exe(Arg)).

exe([])->
    [];
exe([Tuple|Tail]) ->
    [p(Tuple) | exe(Tail)].

p(Tup)->
    case element(3, Tup) of
        [A] when is_binary(A) ->
            btl(A);
	[B] when is_tuple(B) andalso element(1,B) == <<"p">> ->
	    btl(p(B));
	[C] when is_tuple(C) andalso element(1,C) == <<"strong">> ->
	    btl(p(C));
	[D] when is_tuple(D) andalso element(1,D) == <<"div">> ->
	    btl(p(D));
	[E] when is_tuple(E) andalso element(1,E) == <<"pre">> ->
	    btl(p(E));
	[F] when is_tuple(F) andalso element(1,F) == <<"span">> ->
	    btl(p(F));
	[G] when is_tuple(G) andalso element(1,G) == <<"ul">> ->
	    btl(p(G));
	[H] when is_tuple(H) andalso element(1,H) == <<"li">> ->
	    btl(p(H));
	[I] when is_tuple(I) andalso element(1,I) == <<"a">> ->
	    btl(p(I));
	[J] when is_tuple(J) andalso element(1,J) == <<"dt">> ->
	    btl(p(J));
	[K] when is_tuple(K) andalso element(1,K) == <<"dd">> ->
	    btl(p(K));
	[L] when is_tuple(L) andalso element(1,L) == <<"dl">> ->
	    btl(p(L));
	[M] when is_tuple(M) andalso element(1,M) == <<"br">> ->
	    btl(p(M));
        Z = [_|_] ->
            lists:append(loop_list_content(Z));
        _ ->
	    " "
    end.

loop_list_content([])->
    [];
loop_list_content([Head|Tail]) when is_binary(Head) ->
    [btl(Head)|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"li">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"ul">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1, Head) == <<"dt">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1, Head) == <<"dl">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1, Head) == <<"div">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1, Head) == <<"dd">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"strong">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"a">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"pre">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"p">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"br">> ->
    [btl(p(Head))|loop_list_content(Tail)];
loop_list_content([Head|Tail]) when element(1,Head) == <<"span">> ->
    [btl(p(Head))|loop_list_content(Tail)].

print(Arg)->
    io:format("~s~n", [lists:append(exe(Arg))]).
loop_print([])->
    ok;
loop_print([Head|Tail]) ->
    io:format("~s~n", [Head]),
    loop_print(Tail).

%% clean(Des) ->
%%     A = re:replace(Des, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
%%     re:replace(A, "\n        ", "\\ ", [global,{return,list}]).

btl(Arg)->
    case is_binary(Arg) of
	true ->
	    binary:bin_to_list(Arg);
	_ ->
	    Arg
    end.






