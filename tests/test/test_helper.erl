-module(test_helper).

-import(ecouch).
-import(http).
-import(ct).
-export([
    recreate_db/0,
    get/1, get/2, get/3,
    delete/1, delete/2, delete/3,
    put/2, put/3, put/4,
    server_url/0,
    make_url/3, make_url/2, make_url/1
]).


recreate_db() ->
    {ok, _} = delete("ecouch_ct_test"),
    {ok, {{_,201,_}, _, _}} =
        http:request(put, {make_url("ecouch_ct_test"), [], "application/javascript", []}, [],[]).

get(DatabaseName) ->
    get(DatabaseName, "", []).
    
get(DatabaseName, DocId) ->
    get(DatabaseName, DocId, []).
    
get(DatabaseName, DocId, Parameters) ->
    Url = make_url(DatabaseName, DocId, Parameters),
    http:request(get, {Url, []}, [], []).

put(DatabaseName, Body) ->
    put(DatabaseName, "", [], Body).
put(DatabaseName, DocId, Body) ->
    put(DatabaseName, DocId, [], Body).
put(DatabaseName, DocId, Parameters, Body) ->
    Url = make_url(DatabaseName, DocId, Parameters),
    http:request(put, {Url, [], "application/javascript", Body}, [], []).

delete(DatabaseName) ->
    delete(DatabaseName, "", []).
    
delete(DatabaseName, DocId) ->
    delete(DatabaseName, DocId, []).
    
delete(DatabaseName, DocId, Parameters) ->
    Url = make_url(DatabaseName, DocId, Parameters),

    http:request(delete, {Url, []}, [], []).

server_url() ->
    Credentials = case application:get_env(ecouch, user) of
            {ok, User} -> case application:get_env(ecouch, pass) of
                    {ok, Pass} -> User ++ ":" ++ Pass ++ "@";
                    _ -> User ++ "@"
                end;
            _ -> ""
        end,
    {_, Host} = application:get_env(ecouch, host),
    {_, Port} = application:get_env(ecouch, port),
   "http://" ++ Credentials ++ Host ++ ":" ++ Port.

make_url(DatabaseName) ->
    make_url(DatabaseName, "", []).

make_url(DatabaseName, DocId) ->
    make_url(DatabaseName, DocId, []).

make_url(DatabaseName, DocId, Parameters) ->
    Host = server_url(),
    Url = case DocId of
            "" -> "";
            _ -> "/" ++ DocId
        end,
    Host ++ "/" ++ DatabaseName ++ Url ++ params_to_string(Parameters).

params_to_string(TupleList) ->
    case params_to_string(TupleList, "") of
        "" -> "";
        Str -> "?" ++ Str
    end.
    
params_to_string([], Acc) ->
    Acc;
params_to_string([{Key, Value}|TupleList], Acc) ->
    params_to_string(TupleList, lists:flatten(io_lib:format("~s=~s&~s", [Key,Value,Acc]))).
