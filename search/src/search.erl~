-module(getMod).
-author('DoanTalent').
-export([get_html/0]).   
-compile(export_all).

get_html()->
    {ok, Temp} = file:open("data.txt",write),    
    Bd = case httpc:request("http://erlang.org/doc/man/lists.html") of
               {ok, {{_, 200, _}, _, Body}} ->
                   Body;
               {error, Reason} ->
                   Reason
           end,
    Parse_body = mochiweb_xpath:execute("//div/div/div/p",
                                        mochiweb_html:parse(Bd)),
    P = parse_func(Parse_body),
    %% io:format("Length: ~p~n",[length(P)]),
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
%% analyse
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
    Args_raw = mochiweb_xpath:execute("/p/span", Func), 
    Args = parse_in_out(analyse_type(Args_raw)),
    Type_raw = mochiweb_xpath:execute("/p/div/div/span", Func),
    Type = case Type_raw of
               [D] ->
                   element(3,D);
               _ ->
                   analyse_type(Type_raw)
           end,
    {Name, Args, Type}.

%% -----------------------------------------------------------------------------
%% analyse
%% -spec analyse_type(Type) ->  Type
%%       Type :: tuple()
%%       Type :: [T]
%%                                              
%% -----------------------------------------------------------------------------

analyse_type([]) ->
    [];
analyse_type([Head|Tail]) ->
    case element(3,Head) of 
        [Value] -> 
            [Value | analyse_type(Tail)];
        [] ->   analyse_type(Tail)   
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
%%       Prot :: prototype(), 
%%       Input :: ListOfLists|[]
%%       Output :: output_of_func()|[]
%% 
%% -----------------------------------------------------------------------------

parse_in_out(Prot) ->
    RE = ".*\\((.*?)\\) -> (.*?)",
    Split = re:split(Prot,RE),
    {re:split(lists:nth(2,Split), ", "),lists:nth(4,Split)}.

doan()->
    Nameargs = <<"List2">>,
    Type1 =  <<"List1 = List2 = [T]">>,
    Type2 =  <<"Pred = fun((Elem :: T) -> boolean())">>,            
    binary:match(Type2,Nameargs).
    

