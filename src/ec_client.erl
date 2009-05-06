%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%

%%%-------------------------------------------------------------------
%%% @private
%%% File:      ec_client.erl
%%% @author    Vitor Rodrigues <> []
%%% @copyright 2008 Vitor Rodrigues
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2008-04-02 by Vitor Rodrigues
%%%-------------------------------------------------------------------
-module(ec_client).
-author('Vitor Rodrigues').

-behaviour(gen_server).

%% API
-export([start_link/0, query_string/1, query_string/2, url_encode/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
 %%--------------------------------------------------------------------
 %% macro definitions
 %%--------------------------------------------------------------------
 -define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({Operation, Host, Port, User, Pass, From}, State) ->
    case Operation of
        {get, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            Reply = http_g_request(Url, User, Pass),
            gen_server:reply(From, Reply),
            {stop, normal, State};
        {post, Path, Doc} ->
            Url = lists:flatten(io_lib:format("http://~s:~s~s", [Host, Port, Path])),
            Reply = http_p_request(post, Url, Doc, User, Pass),
            gen_server:reply(From, Reply),
            {stop, normal, State};
        {post, Path, Doc, ContentType, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            Reply = http_p_request(post, Url, Doc, ContentType, User, Pass),
            gen_server:reply(From, Reply),
            {stop, normal, State};
        {put, Path, Doc} ->
            Url = lists:flatten(io_lib:format("http://~s:~s~s", [Host, Port, Path])),
            Reply = http_p_request(put, Url, Doc, User, Pass),
            gen_server:reply(From, Reply),
            {stop, normal, State};
        {delete, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            Reply = http_d_request(Url, User, Pass),
            gen_server:reply(From, Reply),
            {stop, normal, State};
        _Other ->
            gen_server:reply(From, {error, "Bad operation"}),
            {stop, normal, State}
    end.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

query_string(Options) ->
    query_string(Options, "?", []).
query_string(Key, Value)->
    query_string([{Key, Value}], "?", []).
query_string([], _Separator, Acc) ->
    lists:flatten(lists:reverse(Acc));
query_string([{Name, Value} | T], Separator, Acc) when is_integer(Value) ->
    query_string([{Name, integer_to_list(Value)} | T], Separator, Acc);
query_string([{Name, Value} | T], Separator, Acc) ->
    UrlName = url_encode(lists:flatten(io_lib:format("~s", [Name]))),
    UrlValue = url_encode(lists:flatten(io_lib:format("~s", [Value]))),
    O = lists:flatten(io_lib:format("~s~s=~s", [Separator, UrlName, UrlValue])),
    query_string(T, "&", [O | Acc]).

% http://erlyaws.svn.sourceforge.net/viewvc/erlyaws/trunk/yaws/src/yaws_api.erl
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        H == $& ->
            [$%, $2, $6 | url_encode(T)];
        H == $= ->
            [$%, $3, $D | url_encode(T)];
        H == $? ->
            [$%, $3, $F | url_encode(T)];
        H == $\  ->
            [$%, $2, $0 | url_encode(T)];
        H == $' ->
            [$%, $2, $7 | url_encode(T)];
        H == $" ->
            [$%, $2, $2 | url_encode(T)];
        H == $\\ ->
            [$%, $5, $C | url_encode(T)];
        H == $( ->
            [$%, $2, $8 | url_encode(T)];
        H == $) ->
            [$%, $2, $9 | url_encode(T)];
        true ->
            case erlang:list_to_integer([H], 16) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

make_headers(none, _) ->
    [];
make_headers(User, Pass) ->
    [{"Authorization", "Basic " ++ 
      base64:encode_to_string(User ++ ":" ++ Pass)}].

http_p_request(Method, Url, Body, User, Pass) ->
    http_p_request(Method, Url, Body, "application/json", User, Pass).
http_p_request(Method, Url, Doc, ContentType, User, Pass) ->
    Headers = make_headers(User, Pass),
    case http:request(Method, {Url, Headers, ContentType, Doc}, [], []) of
        {ok, {_Status, _Header, Body}} ->            
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
http_g_request(Url, User, Pass) ->
    Headers = make_headers(User, Pass),
    case http:request(get, {Url, Headers}, [], []) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
http_d_request(Url, User, Pass) ->
    Headers = make_headers(User, Pass),
    case http:request(delete, {Url, Headers}, [], []) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
