%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(emysql_sup).
-behaviour(supervisor).

-export([start_link/0 ,init/1, spawn/1, start_emysql_conn/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 10}, [
		{emysql_statements, {emysql_statements, start_link, []}, permanent, 5000, worker, [emysql_statements]},
		{emysql_conn_mgr, {emysql_conn_mgr, start_link, []}, permanent, 5000, worker, [emysql_conn_mgr]},
    {emysql_conn_sup, {supervisor, start_link, [{local, emysql_conn_sup}, ?MODULE, [emysql_conn_sup]]},
     transient, infinity, supervisor, [emysql_conn]}
	]}};

init([emysql_conn_sup]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{undefined, {?MODULE, start_emysql_conn, []}, temporary, 5000, worker, [emysql_conn]}]
         }}.

spawn(Fun) ->
    {ok, PId} = supervisor:start_child(emysql_conn_sup, [Fun]),
    PId.

start_emysql_conn(Fun) ->
    Val = proc_lib:spawn_link(Fun),
    {ok, Val}.
    


