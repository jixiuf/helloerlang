-record(room,{room_name,client_socket_id}).
%% -record(users,{id,name,nickname,password}).
-record(users,{name,nickname,password}).

%记录当前有哪些活动用户，最后一次活动时间,如果发现用户长时间 不活动，
%% 可以从此表中删除此记录，及socket 信息清理等
%%registered 表明当前活动的用户是否是已注册用户(true/false) ，因为 ，用户可以匿名登陆
%%匿名用户可以使用已注册的用户名，但是优先级较低，即，已注册用户可以挤掉同名匿名用户
%% 模访irc
-record(activated_user,{name,registered,client_pid,client_socket_id,update_time }).
