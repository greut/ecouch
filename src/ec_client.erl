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
-export([start_link/0]).

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
handle_cast({Operation, Host, Port, From}, State) ->
    case Operation of
        {get, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:append(["http://", Host, ":", Port, Path, QueryString]),
            Reply = http_g_request(Url),
            gen_server:reply(From, Reply),
            {stop, "Normal", State};
        {post, Path, Doc} ->
            Url = lists:append(["http://", Host, ":", Port, Path]),
            Reply = http_p_request(post, Url, Doc),
            gen_server:reply(From, Reply),
            {stop, "Normal", State};
        {post, Path, Doc, ContentType, Options} ->
            QueryString = query_string(Options),
            Url = lists:append(["http://", Host, ":", Port, Path, QueryString]),
            Reply = http_p_request(post, Url, Doc, ContentType),
            gen_server:reply(From, Reply),
            {stop, "Normal", State};
        {put, Path, Doc} ->
            Url = lists:append(["http://", Host, ":", Port, Path]),
            Reply = http_p_request(put, Url, Doc),
            gen_server:reply(From, Reply),
            {stop, "Normal", State};
        {delete, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:append(["http://", Host, ":", Port, Path, QueryString]),
            Reply = http_d_request(Url),
            gen_server:reply(From, Reply),
            {stop, "Normal", State};
        _Other ->
            gen_server:reply(From, {error, "Bad operation"}),
            {stop, "Normal", State}
    end,
    {noreply, State}.

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
query_string([], _Separator, Acc) ->
    lists:flatten(Acc);
query_string([{Name, Value} | T], Separator, Acc) ->
    O = lists:append([Separator, Name, "=", Value]),
    query_string(T, "&", [O | Acc]).

http_p_request(Method, Url, Body) ->
    http_p_request(Method, Url, Body, "application/json").
http_p_request(Method, Url, Doc, ContentType) ->
    case http:request(Method, {Url, [], ContentType, Doc}, [], []) of
        {ok, {_Status, _Header, Body}} ->            
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
http_g_request(Url) ->
    case http:request(Url) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
http_d_request(Url) ->
    case http:request(delete, {Url, []}, [], []) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.