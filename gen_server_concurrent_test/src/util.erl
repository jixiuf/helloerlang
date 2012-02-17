% -*- coding:utf-8 -*-

-module(util).
-export([pid_msg_info/1]).


%%得到某进程未处理的消息数量，
pid_msg_info(PidOrName) when is_pid(PidOrName)->
    {message_queue_len,Len}= erlang:process_info(PidOrName,message_queue_len),
    {heap_size,Heap_size}= erlang:process_info(PidOrName,heap_size),
    {total_heap_size,Total_heap_size}= erlang:process_info(PidOrName,total_heap_size),
    io:format(" pid : ~p ,{message_queue_length,head_size,totle_head_size}:{~p,~p,~p}~n",[PidOrName,Len,Heap_size,Total_heap_size]);
pid_msg_info(PidOrName) when is_atom(PidOrName) ->
    {message_queue_len,Len}= erlang:process_info(whereis(PidOrName),message_queue_len),
    {heap_size,Heap_size}= erlang:process_info(whereis(PidOrName),heap_size),
    {total_heap_size,Total_heap_size}= erlang:process_info(whereis(PidOrName),total_heap_size),
    io:format(" pid : ~p ,{message_queue_length,head_size,totle_head_size}:{~p,~p,~p}~n",[PidOrName,Len,Heap_size,Total_heap_size])
        .
