-module(tet).
-export([insert/0,query_user/1]).


-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").


%% qlc_query_user(Username)->
%%     Fun= fun()->
%%                  io:format("query by qlc~n",[]),
%%                   Q=qlc:q([User|| User <- mnesia:table(user),User#user.name==Username]),
%%                  qlc:e(Q)
%%          end,
%%     {atomic,Result} =mnesia:transaction(Fun),
%%     lists:map(fun(U)->
%%                       io:format("useriname =~p ~n",[U#user.name])
%%               end,
%%               Result)
%%         .
query_user(UserId)->
    Fun = fun()->
                  Results=mnesia:read(users,UserId),  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
                  lists:map(fun(Result)->
                                    io:format("userid=~p,username=~p,password=~p ~n",[1,Result#users.name,Result#users.password])
                            end,
                            Results)
          end,
    mnesia:transaction(Fun)
        .
insert()->
    Fun = fun()->
                  User=#users{name="jixf2",password=self(),nickname="jixf2"},
                  mnesia:write(User)
          end,
    mnesia:transaction(Fun)
        .
