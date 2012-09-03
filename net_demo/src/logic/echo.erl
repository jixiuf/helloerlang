-module(echo).
-export([cmd_echo/1]).


-include_lib("../base_header.hrl").

cmd_echo(Msg)->
    %% 逻辑处理很简单，只是简单的将Msg返回给客户端
    %% 这里，resp是一个list,
    %% 客户端发来一条请求，服务器给它返回的协议可能不只一条，
    %% 比如，客户端发来一条购买请求， 服务器要给它返回
    %% 两个更新
    %% 1扣除money
    %% 2往背包里加入所买的物品
    %%对这两个response ，一起返回到代码上层， 上层将这两条协议一起发给客户端 ，客户端
    %% 接到响应后，分别做出 扣money,加物品的响应即可
    [server_util:build_resp(?S2C_PROTOCOL_ECHO,#s2c_echo{msg=Msg})]
        .
