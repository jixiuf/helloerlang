-record(room,{room_name,client_socket_id}).
%% -record(user,{id,name,nickname,password}).
-record(user,{name,nickname,password}).

%记录当前有哪些活动用户，最后一次活动时间,如果发现用户长时间 不活动，
%% 可以从此表中删除此记录，及socket 信息清理等
-record(activated_user,{userid,client_socket_id,update_time }).
