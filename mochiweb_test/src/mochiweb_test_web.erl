%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for mochiweb_test.

-module(mochiweb_test_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE},{max,10000}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "time" ->%% 新增了 /time 这个 URL，它是一个 HTTP Chunked 的例子
                        Response = Req:ok({"text/plain", chunked}),

                        Params = Req:parse_qs(), %get query string
                        Id = proplists:get_value("id", Params), % http://localhost:8080/time?id=1
                        time(Response,Id);
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.



%% 打印当前时间，间隔一秒，再在已经打开的 http 连接之上，再次打印，这也就是所谓 HTTP长连接/ServerPush 的一种
time(Resp,Id)->
    case Id of
        undefined->
            Resp:write_chunk(io_lib:format("The time for Id:~p is: ~p~n",[0 ,calendar:local_time()])),
            io:format("~p~n",[io:format("The time for Id:~p is: ~p~n",[0 ,calendar:local_time()])]);
        _ ->
            Resp:write_chunk(io_lib:format("The time for Id:~p is: ~p~n",[Id ,calendar:local_time()])),
            io:format("~p~n",[io:format("The time for Id:~p is: ~p~n",[Id ,calendar:local_time()])])
    end,
    timer:sleep(1000),
    time(Resp,Id).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
