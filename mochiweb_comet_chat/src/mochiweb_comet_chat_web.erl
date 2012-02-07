%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chat.

-module(mochiweb_comet_chat_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%20s 不活动，则认为用户已经离开页面，向客户端发送一个超时信息，
%% 其实，对于客户端来说，这个超时信息与普通信息没区别，
%% 如果并不是真的超时（因客户关闭网页）,大可把它当成正常信息处理
%%即再向服务器端发送一条请求信息。
-define(TIMEOUT, 20000).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

room(Users) ->
    receive
        {From, subscribe} ->
            From ! subscribed,
            io:format("~p~n",[[From | Users]]) ,
            room([From | Users]);
        {From, unsubscribe} ->
            From ! unsubscribed,
            io:format("~p~n",[Users -- [From]]) ,
            room(Users -- [From]);
        {From, post, Message} ->
            From ! posted,
            lists:foreach(fun(User) ->
                                                % broadcast the message
                                  User ! Message
                          end, Users),
            %% room(Users),
            %% 所有等待的用户都会接收的消息，
            %% 从队列中去掉他们
            room([]),
            io:format("~n",[]) ;
        %% room([]);
        _Any ->
            room(Users)
    end.

get_the_room() ->
    % does the room exists?
    Pid = whereis(theroom),
    if
        is_pid(Pid) ->
            % yup
            Pid;
        true ->
            % create it
            NewPid = spawn(fun() ->
                room([])
            end),
            register(theroom, NewPid),
            NewPid
    end.

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "chat" ->
                    io:format("get ...~n",[]) ,
                    Room = get_the_room(),
                    Room ! {self(), subscribe},
                    receive
                        subscribed ->
                            % subscription is ok
                            % now wait for a message
                            receive
                                Message ->
                                    {Type, Message} = {ok, Message}
                            after ?TIMEOUT ->
                                % we waited too long
                                {Type, Message} = {error, <<"timeout">>}
                            end
                    after 1000 ->
                        % subscription failed on time
                        {Type, Message} = {error, <<"timeout">>}
                    end,

                    case Type of
                        error ->
                            % we need to unsubscribe from the room
                            % because it failed somewhere
                            Room ! {self(), unsubscribe},
                            receive
                                unsubscribed ->
                                    % unsubscribed
                                    ok
                            after 1000 ->
                                % do not wait too long
                                ok
                            end;
                        ok ->
                            % everything went fine
                            ok
                    end,

                    % send back the JSON message
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                {Type, Message}
                            ]
                        })
                    });
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                "chat" ->
                    Data = Req:parse_post(),
                    Room = get_the_room(),
                    % post
                    io:format("message from post:~p~n",[proplists:get_value("message", Data)]) ,
                    Room ! {self(), post, list_to_binary(proplists:get_value("message", Data))},
                    receive
                        posted ->
                            % posted
                            Body = {ok, <<"posted">>}
                    after 1000 ->
                        % something went wrong
                        Body = {error, <<"timeout">>}
                    end,

                    % send back the JSON message
                    Req:ok({"text/javascript", mochijson2:encode({
                            struct, [
                                Body
                            ]
                        })
                    });
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
