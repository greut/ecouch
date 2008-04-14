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
%% $Id$
%%
%% @copyright 2008 Vitor Rodrigues
%% @author Vitor Rodrigues <vitor@tarpipe.com>
%% @version {@version}
%%
%% @doc
%% <h1>Elang API to CouchDb</h1> 
%% eCouch is an application that provides an API to a CouchDb server
%% It uses the <a href="http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang">rfc4627</a> module from <a href="http://www.lshift.net/">LShift</a>
%% The design was inspired in the article <a href="http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles">Building a Non-blocking TCP server using OTP principles</a> 
%% and assumes that <a href="http://www.erlang.org/doc/apps/inets/index.html">inets</a> application is running.
%% todo:
%% Accept a list of servers and implement load distribution algorithms <br/>
%% Implement views
%%
%% @end
%% =====================================================================

-module(ecouch).
-author('Vitor Rodrigues').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init/1, start_client/0]).

%% API
-export([
        db_create/1,
        db_delete/1,
        db_list/0,
        db_info/1,
        doc_create/2,
        doc_create/3,
        doc_bulk_create/2,
        doc_update/3,
        doc_bulk_update/2,
        doc_delete/3,
        doc_get/2,
        doc_get/3,
        doc_get_all/1,
        doc_get_all/2,
        view_create/2,
        view_update/3,
        view_delete/2,
        view_get/1,
        view_get/2,
        view_adhoc/2,
        view_adhoc/3,
        view_access/2,
        view_access/3
        ]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(_Type, StartArgs::startargs()) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%%
%% @type startargs() = {host(), tcp_port()}
%% @type host() = string()
%% @type tcp_port() = int()
%%
%% @doc This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end 
%%--------------------------------------------------------------------

start(_Type, {Host, Port}) ->
    case get_app_opt(host, Host) of
        none ->
            {error, "Missing required config option 'host'"};
        HostVal ->
            case get_app_opt(port, Port) of
                none ->
                    {error, "Missing required config option 'port'"};
                PortVal ->
                    supervisor:start_link({local, ?MODULE}, ?MODULE, [HostVal, PortVal])
        end
    end.
    
%% @hidden

start_client() ->
    supervisor:start_child(ec_client_sup, []).


%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% @end 
%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%% @hidden
    
init([Host, Port]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % eCouch Listener
              {   ec_listener_sup,                         % Id       = internal id
                  {ec_listener,start_link,[Host, Port]},   % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [ec_listener]                            % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   ec_client_sup,
                  {supervisor,start_link,[{local, ec_client_sup}, ?MODULE, [ec_client]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

%% @hidden

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % HTTP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% API Functions

%% @spec db_create(DatabaseName::string()) -> ok | {error, Reason::term()}
%%
%% @doc Create a database

db_create(DatabaseName) ->
    Path = lists:append(["/", DatabaseName, "/"]),
    Reply = gen_server:call(ec_listener, {put, Path, []}),
    handle_reply(Reply).

%% @spec db_delete(DatabaseName::string()) -> ok | {error, Reason::term()}
%%
%% @doc Delete a database

db_delete(DatabaseName) ->
    Path = lists:append(["/", DatabaseName, "/"]),
    Reply = gen_server:call(ec_listener, {delete, Path, []}),
    handle_reply(Reply).

%% @spec db_list() -> ok | {error, Reason::term()}
%%
%% @doc List databases
    
db_list() ->
    Path = "/_all_dbs",
    Reply = gen_server:call(ec_listener, {get, Path, []}),
    handle_reply(Reply).

%% @spec db_info(DatabaseName::string()) -> {ok, Info::json()} | {error, Reason::term()}
%%
%% @type json() = obj() | array() | num() | str() | true | false | null
%% @type obj() = {obj, [{key(), val()}]}
%% @type array() = [val()]
%% @type key() = str() | atom()
%% @type val() = obj() | array() | num() | str() | true | false | null
%% @type num() = int() | float()
%% @type str() = bin()
%% 
%% @doc Database info

db_info(DatabaseName) ->
    Path = lists:append(["/", DatabaseName]),
    Reply = gen_server:call(ec_listener, {get, Path, []}),
    handle_reply(Reply).

%% @spec doc_create(DatabaseName::string(), Doc::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Create document

doc_create(DatabaseName, Doc) ->
    DocJson = rfc4627:encode(Doc),
    Path = lists:append(["/", DatabaseName, "/"]),
    Reply = gen_server:call(ec_listener, {post, Path, DocJson}),
    handle_reply(Reply).

%% @spec doc_create(DatabaseName::string(), DocName::string(), Doc::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Create a named document
    
doc_create(DatabaseName, DocName, Doc) ->
    JsonDoc = rfc4627:encode(Doc),
    Path = lists:append(["/", DatabaseName, "/", DocName]),
    Reply = gen_server:call(ec_listener, {put, Path, JsonDoc}),
    handle_reply(Reply).

%% @hidden

doc_bulk_create(_DatabaseName, _DocList) ->
    {error, "Not implemented"}.
    
%% @spec doc_update(DatabaseName::string(), DocName::string(), Doc::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Update document

doc_update(DatabaseName, DocName, Doc) -> 
    doc_create(DatabaseName, DocName, Doc).

%% @hidden

doc_bulk_update(_DatabaseName, _DocListRev) ->
    {error, "Not implemented"}.

%% @spec doc_delete(DatabaseName::string(), DocName::string(), Rev::string()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Delete document
    
doc_delete(DatabaseName, DocName, Rev) ->
    Path = lists:append(["/", DatabaseName, "/", DocName]),
    Reply = gen_server:call(ec_listener, {delete, Path, [{"rev", Rev}]}),
    handle_reply(Reply).

%% @spec doc_get(DatabaseName::string(), DocName::string) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get document

doc_get(DatabaseName, DocName) ->
    doc_get(DatabaseName, DocName, []).

%% @spec doc_get(DatabaseName::string(), DocName::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get document

doc_get(DatabaseName, DocName, Options) ->
    Path = lists:append(["/", DatabaseName, "/", DocName]),
    Reply = gen_server:call(ec_listener, {get, Path, Options}),
    handle_reply(Reply).

%% @spec doc_get_all(DatabaseName::string()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get all documents

doc_get_all(DatabaseName) ->
    doc_get_all(DatabaseName, []).
    
%% @spec doc_get_all(DatabaseName::string(), Options::options()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%% @doc Get all documents

doc_get_all(DatabaseName, Options) ->
    Path = lists:append(["/", DatabaseName, "/_all_docs"]),
    Reply = gen_server:call(ec_listener, {get, Path, Options}),
    handle_reply(Reply).

%% @hidden

view_create(_ViewName, _Funs) ->
    {error, "Not implemented"}.

%% @hidden

view_update(_ViewName, _Funs, _Rev) ->
    {error, "Not implemented"}.

%% @hidden

view_delete(_ViewName, _Rev) ->
    {error, "Not implemented"}.

%% @hidden

view_get(_ViewName) ->
    {error, "Not implemented"}.

%% @hidden

view_get(_ViewName, _Rev) ->
    {error, "Not implemented"}.

%% @spec view_adhoc(DatabaseName::string(), Fun::json()) -> {ok, Response::json()} | {error, Reason::term()}
%%
%%
%% @doc Access an adhoc view

view_adhoc(DatabaseName, Fun) ->
    view_adhoc(DatabaseName, Fun, []).

%% @spec view_adhoc(DatabaseName::string(), Fun::json(), Options::options()) -> {ok, Response::json} | {error, Reason::term()}
%%
%% @doc Access an adhoc view

view_adhoc(DatabaseName, Fun, Options) ->
    Path = lists:append(["/", DatabaseName, "/_temp_view"]),
    Reply = gen_server:call(ec_listener, {post, Path, Fun, "text/javascript", Options}),
    handle_reply(Reply).

%% @hidden

view_access(DatabaseName, ViewName) ->
    view_access(DatabaseName, ViewName, []).

%% @hidden

view_access(_DatabaseName, _ViewName, _Options) ->
    {error, "Not implemented"}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_reply(Reply) ->
    case Reply of
        {error, Reason} ->
            {error, Reason};
        R ->
              case rfc4627:decode(R) of
                  {ok, Json, _Raw} ->
                      {ok, Json};
                  {error, Reason} ->
                      {error, Reason}
              end
    end.

get_app_opt(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
            [[Val | _]] -> Val;
            error       -> Default
            end
        end.
