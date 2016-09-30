-module(search).
-author('DoanTalent').

%% -----------------------------------------------------------------------------
%% INCLUDE
%% -----------------------------------------------------------------------------
-include_lib("stdlib/include/qlc.hrl").
-include("../inc/search.hrl").

%% -----------------------------------------------------------------------------
%% EXPORT
%% -----------------------------------------------------------------------------

-export([get/0,apps/0]).   
-compile(export_all).

%% -----------------------------------------------------------------------------
%% DEFINE
%% -----------------------------------------------------------------------------

-define(APPS,"http://erlang.org/doc/applications.html").
-define(LISTS,"http://erlang.org/doc/man/lists.html").
-define(DOC,"http://erlang.org/doc/").
-define(MAN,"http://erlang.org/doc/man/").
-define(TIMEOUT, 300000).
%% -----------------------------------------------------------------------------
%% Function
%% -----------------------------------------------------------------------------

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    %% mnesia:create_table(app,
    %%                     [{attributes, record_info(fields, app)}]),
    %% mnesia:create_table(func,
    %%                     [{attributes, record_info(fields, func)},{type, bag}]),
    mnesia:create_table(mod,
                        [{attributes, record_info(fields, mod)},{type, set}]).

insert(Name) ->
    mnesia:transaction(fun() ->
                               mnesia:write(Name)
                       end).
%% -----------------------------------------------------------------------------
%% SAVE APPLICATION
%% -----------------------------------------------------------------------------

%% apps()->
%%     {ok, Temp} = file:open("data.txt",write), 
%%     Bd = get_body(?APPS),
%%     Parse_body = mochiweb_xpath:execute("/html/body/center/table/tr/td/table/tr",
%%                                         Bd),
%%     io:format(Temp,"~p~n",[parse_apps_name(Parse_body)]),
%%     ok.  
apps()->
    %% {ok, Temp} = file:open("data.txt",write), 
    Bd = get_body(?APPS),
    Parse_body = mochiweb_xpath:execute("/html/body/center/table/tr",
                                        Bd),
    Data = save_app(Parse_body),
    io:format("~p~n",[Data]).

save_app([])->
    [];
save_app([Head|Tail]) ->
    case mochiweb_xpath:execute("/tr/@class", Head) of
        [<<"app">>] ->
            [Name,Version] = return_value3(mochiweb_xpath:execute("/tr/td/table/tr/td/a", Head)),
            [Des] = element(3,lists:nth(2,mochiweb_xpath:execute("/tr/td", Head))),
            App = #app{name = binary:bin_to_list(Name),
                     version = binary:bin_to_list(Version),
                     des = clean_des(Des)},
            insert(App),
            save_app(Tail);
        _ ->
            save_app(Tail)
    end.
%% -----------------------------------------------------------------------------
%% SELECT APPS
%% -----------------------------------------------------------------------------

select_app() ->
    {atomic, Row} = mnesia:transaction( 
                      fun() ->
                                 qlc:eval( qlc:q(
                                             [ X || X <- mnesia:table(app) ])) 
                      end 
                     ),
    loop_print_app(Row). 

select_app(Arg) when is_atom(Arg) ->
    Name = atom_to_list(Arg),
    Fun = 
        fun() ->
            mnesia:read({app, Name})
        end,
    {atomic, [Row]}=mnesia:transaction(Fun),
    io:format("~s ~s ~n ~s ~n ", [Row#app.name, Row#app.version, Row#app.des] ).

%% -----------------------------------------------------------------------------
%% SAVE MODULE
%% -----------------------------------------------------------------------------


apps_mod()  ->
    %% {ok, Temp} = file:open("data.txt",write), 
    Bd = get_body(?APPS),
    Parse_body = mochiweb_xpath:execute("/html/body/center/table/tr",
                                        Bd),
    %% UrlAll = nonDuplicate_list(mochiweb_xpath:execute("/html/body/center/table/tr/td/table/tr/td/a/@href",Bd)),
    Listapp = return_list_app(Parse_body),
    write_mod(Listapp),
    %% Listdesmod = return_list_des_mod(ModAll),
    %% io:format(Temp,"~p~n~p~n~p~n",[Listapp, Save,length(Save)]),
    {ok,saved}.

return_list_des_mod([])->
    [];
return_list_des_mod([Name|Tail]) ->
    case binary:match(Name, <<"App">>) of
	nomatch ->
	    Bd = get_body(?MAN++binary:bin_to_list(Name)++".html"),
	    Des = element(3, lists:nth(1,mochiweb_xpath:execute("/html/body/div/div/div/div/p", Bd))),
	    [Des|return_list_des_mod(Tail)];
	_ ->
	    return_list_des_mod(Tail)
    end.


return_list_app([])->
    [];
return_list_app([Head|Tail]) ->
    case mochiweb_xpath:execute("/tr/@class", Head) of
        [<<"app">>] ->
            [Name,_] = return_value3(mochiweb_xpath:execute("/tr/td/table/tr/td/a", Head)),
            [clean(Name)|return_list_app(Tail)];
        _ ->
            return_list_app(Tail)
    end.

%% -----------------------------------------------------------------------------
%% SAVE MODULE FOR EACH APPLICATION
%% -----------------------------------------------------------------------------

write_mod(Apps) ->
    save_all_mod(Apps, 5),
    Results = wait_for_responses([]),
    Remain = [App || App <- Apps, lists:member(App, Results) == false],
    loop_print(Remain).

save_all_mod([],_)->
    ok;
save_all_mod([_App|Tail], 0) ->
    receive
    after 1000 ->
            save_all_mod(Tail,5)
    end;
save_all_mod([App|Tail], N) ->
    spawn_workers(App),
    save_all_mod(Tail, N - 1).

work(App, Master) ->
    Url = "apps/"++binary:bin_to_list(App)++"/index.html",
    Bd = get_body(?DOC++Url),
    Listmodule = mochiweb_xpath:execute("/html/body/div/div/div/ul/li/@title", Bd),
    save_mod(Listmodule, App),
    Master ! {ok, App}.

spawn_workers(App)->
    spawn(search, work, [App, self()]).


wait_for_responses(State)->
    receive
        {ok,A} ->
            NewState = add_state(State,A),
            wait_for_responses(NewState)
    after 
           300000 ->
               State
    end.
add_state(State,A) ->
    State++[A].
 
%% save_all_mod([])->
%%     [];
%% save_all_mod([App|Tail]) ->
%%     Url = "apps/"++binary:bin_to_list(App)++"/index.html",
%%     Bd = get_body(?DOC++Url),
%%     Listmodule = mochiweb_xpath:execute("/html/body/div/div/div/ul/li/@title", Bd),
%%     spawn_workers(Listmodule, App),
%%     save_all_mod(Tail).
    
save_mod([],Name)->
    {ok,inserted,Name};
save_mod([Head|Tail],Name) ->
    Des = case binary:match(clean(Head), <<"App">>) of
    	      nomatch ->
    		  Bd = get_body(?MAN++binary:bin_to_list(clean(Head))++".html"),
    		  Des_raw = element(3, lists:nth(1,mochiweb_xpath:execute("/html/body/div/div/div/div/p", Bd))),
                  des:parse(Des_raw);
    	      _ ->
    		  []
    	  end,
    Mod = #mod{app = clean_des(Name),
               name = clean_des(Head),
               des = Des},
    insert(Mod),
    [Mod|save_mod(Tail,Name)].
    
%% -----------------------------------------------------------------------------
%% SELECT MODULE
%% -----------------------------------------------------------------------------
    
select_mod() ->
    {atomic, Row} = mnesia:transaction( 
                      fun() ->
                                 qlc:eval( qlc:q(
                                             [ X || X <- mnesia:table(mod)])) 
                      end 
                     ),
    loop_print_mod(Row). 

select_mod(Arg) when is_atom(Arg) ->
    Name = atom_to_list(Arg),
    Fun = 
        fun() ->
            mnesia:read({mod, Name})
        end,
    {atomic, [Row]}=mnesia:transaction(Fun),
    io:format("Module: ~s Application: ~s ~nDescription:~n~s~n",[Row#mod.name, Row#mod.app , Row#mod.des]).

%% apps(Name, Module) ->
%%     %% {ok, Temp} = file:open("data.txt",write), 
%%     Bd = get_body(?APPS),    
%%     UrlAll = nonDuplicate_list(mochiweb_xpath:execute("/html/body/center/table/tr/td/table/tr/td/a/@href",
%%                                                Bd)),
%%     Url = lookup_url(UrlAll, binary:list_to_bin(Name)),
%%     Bd2 = get_body(?DOC++binary:bin_to_list(Url)),
%%     Parse_body = mochiweb_xpath:execute("/html/body/div/div/div/ul/li",
%%                                         mochiweb_html:parse(Bd2)),
%%     Mod = lookup_module(Parse_body,Module),
%%     A = mochiweb_xpath:execute("/ul/li/@title",lists:nth(2,element(3,Mod))),
%%     io:format("~p~n",[A]),
%%     ok.

lookup_module([], _)->
    nomatch;
lookup_module([Head|Tail], Name) ->
    [Title] = mochiweb_xpath:execute("/li/@title", Head),                                             
    case binary:match(Title, Name) of 
        nomatch -> 
            lookup_module(Tail, Name);
        _  ->
            Head
    end.

lookup_url([],_) ->
    nomatch;
lookup_url([Head|Tail], Name) ->
    case binary:match(Head, Name) of 
        nomatch -> 
            lookup_url(Tail, Name);
        _  ->
            Head
    end.


%% -----------------------------------------------------------------------------
%% return Name and Version of app
%% -----------------------------------------------------------------------------

parse_app_name(Name)->
    return_value3(mochiweb_xpath:execute("/tr/td/a", Name)).

parse_apps_name([])->
    [];
parse_apps_name([Name|Tail])->
    A = return_value3(mochiweb_xpath:execute("/tr/td/a", Name)), 
    [A|parse_apps_name(Tail)].

%% -----------------------------------------------------------------------------
%% get data for func search
%% -----------------------------------------------------------------------------

get()->
    {ok, Temp} = file:open("data.db",write),    
    Bd = get_body(?LISTS),
    Parse_body = mochiweb_xpath:execute("/div/div/div/p", Bd),
    P = parse_func(Parse_body),
    io:format(Temp,"~p",[P]), 
    ok.

%% -----------------------------------------------------------------------------
%% search
%% -spec seacch(Module, [Args],[Output]) -> Function when
%%       Module :: name_module(), 
%%       [Args] :: ListOfLists|[]
%%       [Output] :: output_of_func()|[]
%%       Function :: atom().                                         
%% -----------------------------------------------------------------------------

%% search(Module, [H_args|T_args],[H_out|T_out])->
%%     {ok, Temp} = file:open("data/"++Module++".html",read),

parse_func([])->
    [];
parse_func([Head|Tail])->
    case mochiweb_xpath:execute("/p/a/@name", Head) of 
        [] ->
            parse_func(Tail);
        _ ->
            Func = analyse_func(Head),
            [Func|parse_func(Tail)]
    end.
    
%% -----------------------------------------------------------------------------
%% analyse_func use for parse from tuple of Func to value. Then we can search. 
%% -spec analyse_func(Func) ->  [Name, Args, Type] when
%%       Func :: tuple()
%%       Name :: binary(atom())
%%       Args :: binary(atom())
%%       Type :: [T]
%%                                              
%% -----------------------------------------------------------------------------

analyse_func(Func)->
    Name = case B = mochiweb_xpath:execute("/p/a/@name", Func) of 
               [A] -> 
                   A;
               _ ->
                   B  
               end,
    %% Args_raw is list of prototype 
    Args_raw = return_value3(mochiweb_xpath:execute("/p/span", Func)), 
    {In,Out}  = parse_in_out(Args_raw),
    Type_raw = mochiweb_xpath:execute("/p/div/div/span", Func),
    Type = case Type_raw of
               [D] ->
                   element(3,D);
               _ ->
                   return_value3(Type_raw)
           end,
    Input = nonDuplicate_list(find_type(In,Type)),
    Output = nonDuplicate_list(find_type(Out,Type)),
    {Name, Input, Output,Type}.

%% -----------------------------------------------------------------------------
%% return_value3
%% -spec return_value3(Type) ->  Type
%%       Type :: tuple()
%%       Type :: [T]
%%                                              
%% -----------------------------------------------------------------------------

return_value3([]) ->
    [];
return_value3([Head|Tail]) ->
    case element(3,Head) of 
        [Value] -> 
            [Value | return_value3(Tail)];
        [] ->   return_value3(Tail)   
    end.
      
%% -----------------------------------------------------------------------------
%% save file *.html into directory data/
%% -----------------------------------------------------------------------------

save_data([],_)->
    io:format("~n"),
    ok;
save_data([Head|Tail],Len) ->
    % print process
    S = trunc(80/Len),
    M = (80 - (trunc(80/Len)*(Len-1))),
    case Tail of
        [] -> print(M);
        _ -> print(S)
        end,
    Module = case T = string:strip(binary_to_list(Head)) of
            "STDLIB (App)" -> "STDLIB_app";
            _ -> T
        end,
    {ok, Temp} = file:open("data/"++Module++".html",write),
    Bd = case httpc:request("http://erlang.org/doc/man/"++Module++".html") of
             {ok, {{_, 200, _}, _, Body}} ->
                 Body;
             {error, Reason} ->
                 Reason
         end,
    io:format(Temp,"~s",[Bd]),
    file:close(Temp),
    save_data(Tail,Len).

%% -----------------------------------------------------------------------------
%% print "*" for process
%% -----------------------------------------------------------------------------

print(1) -> 
    io:format("*");
print(S)->
    io:format("*"),
    print(S-1).

%% -----------------------------------------------------------------------------
%% analyse args
%% -----------------------------------------------------------------------------

define_args(Name)->
    case Name of
        list -> "[T]";
        list1 -> "[T]";
        list2 -> "[T]";
        list3 -> "[T]";
        list4 -> "[T]";
        listoflists -> "[List]";           
        boolean -> "booble()";
        term -> "term()";
        elem -> "T"
    end.
   
%% -----------------------------------------------------------------------------
%% analyse output
%% -----------------------------------------------------------------------------

define_output(Name)->
    case Name of
        boolean -> "boolean()";
        list -> "[T]";
        list1 -> "[T]";
        list2 -> "[T]";
        list3 -> "[T]";
        list4 -> "[T]";
        elem -> "T";
        term -> "term()"
        end.

%% -----------------------------------------------------------------------------
%% -spec parse_in_out(Prot) ->  {Input,Output} when
%%       Prot :: [prototype()], 
%%       Input :: ListOfLists|[]
%%       Output :: [output_of_func()]|[]
%% 
%% -----------------------------------------------------------------------------

parse_in_out([])->
    {[],[]};
parse_in_out([Prot|Tail]) ->
    RE = ".*\\((.*?)\\) -> (.*?)",
    Split = re:split(Prot,RE),
    Output = lists:nth(4,Split),
    Input = re:split(lists:nth(2,Split), ", "),
    {In,Out} = parse_in_out(Tail),     
    {Input++In,[Output]++Out}.

%% -----------------------------------------------------------------------------
%% find type of data for input or output
%% -spec find_type(Name,Type) ->  {Input,Output} when
%%       Name :: [T], 
%%       Input :: [List]|[]
%%       Output :: [List]|[]
%% 
%% -----------------------------------------------------------------------------

find_type([],_)->
    [];
find_type([Head|Tail],Type)->
    [return_type(Head,Type)|find_type(Tail,Type)].

return_type(_,[])->
    [];
return_type(Nameargs,[Type|TailType])->
    case binary:match(Type,Nameargs) of
	nomatch ->
	    return_type(Nameargs,TailType);
	_ ->
	    Split = case A = re:split(Type,".*? = .*?") of
			[] ->
			    [nok, errror];
			_ ->
			    A
		    end,
	    lists:nth(1,lists:reverse(Split))
    end.

%% -----------------------------------------------------------------------------
%% remove duplicate
%% -----------------------------------------------------------------------------

nonDuplicate_list(List) ->
    sets:to_list(sets:from_list(List)).

%% -----------------------------------------------------------------------------
%% get Body parsed from html
%% -----------------------------------------------------------------------------

get_body(Url) ->
    Bd = case httpc:request(Url) of
             {ok, {{_, 200, _}, _, Body}} ->
                 Body;
             {error, Reason} ->
                 Reason
         end,
    mochiweb_html:parse(Bd).

clean_des(Des) ->
    A = re:replace(Des, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
    re:replace(A, "\n", "\\ ", [global,{return,list}]).
clean(Arg)->    
    A = re:replace(Arg, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]),
    re:replace(A, "\n", "\\ ", [global,{return,binary}]).

loop_print([])->
    ok;
loop_print([Head|Tail]) ->
    io:format("~s~n", [Head]),
    loop_print(Tail).

loop_print_app([])->
    [];
loop_print_app([Head|Tail]) ->
    io:format("Application:~s~nDescription:~s~n~n", [Head#app.name, Head#app.des]),
    loop_print_app(Tail).

loop_print_mod([])->
    [];
loop_print_mod([Head|Tail]) ->
    io:format("Module:~s Application:~s~n~n", [Head#mod.name, Head#mod.app]),
    loop_print_mod(Tail).

select_search(Word) -> 
    mnesia:transaction( 
    fun() ->
         qlc:eval( qlc:q(
              [ {F0,F1,F2,F3} || 
                   {F0,F1,F2,F3} <- 
                        mnesia:table(mod),
                        (string:str(F2, Word)>0) or  
                        (string:str(F3, Word)>0)
               ])) 
    end ).

