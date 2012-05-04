http://www.ostinelli.net/boost-message-passing-between-erlang-nodes/
测试 两节点间 send receive 消息需要多长时间

erl -sname pong
start_pong:start().

erl -name ping

start_ping:start('pong@192.168.1.31',1000). %send 1000条
