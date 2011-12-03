-define(server_node,server@jf.org).

-record(logon,{username,from_pid}).
-record(message2,{to_name,from_pid,msg}).
-record(message_send,{from_pid,msg}).
-record(message_req,{to_name,msg}).
