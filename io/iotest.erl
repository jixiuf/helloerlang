%%    -*- coding:utf-8 -*-
-module(iotest).
-export([write/0,read_line/0,read_all_file/0,write_file/0,read_file_line_by_line/0,consult/0]).

read_line()->
    {ok,Fh}= file:open("iotest.erl",read),
    Line= io:get_line(Fh,''),
    io:format("~p~n",[Line]),
    Line2= io:get_line(Fh,''),
    io:format("~p~n",[Line2]),
    file:close(Fh)
        .
%%用io:get_line() 统计行数速度很慢
read_file_line_by_line()->
    io:format("read file line by line ...~n",[]),
    {ok,Fh}= file:open("iotest.erl",read),
    FirstLine= io:get_line(Fh,''),
    scan_line(FirstLine,Fh),
    file:close(Fh)
        .

scan_line(eof,_File)->
    io:format("end of line~n",[]);
scan_line(Line,File) ->
    io:format("~p~n",[Line]),
    scan_line(io:get_line(File,''),File)
        .


read_all_file()->
    {ok,Bin}= file:read_file("iotest.erl"),
    io:format("~p~n",[Bin])
        .
%%写入整个文件
write_file()->
    io:format("write file ~n",[]),
    {ok,Bin}= file:read_file("iotest.erl"),
    file:write_file("out.dat",Bin)
    .
write()->
    io:format("write~n",[]),
    {ok,Fh}= file:open("out.dat",write),

    file:write(Fh,"helo"),

    file:close(Fh)
    .

consult()->
     %% {ok,Term} = file:consult("test.dat")
     {ok,[{key1,Val}]} = file:consult("test.dat"),
        io:format("value of key1 is ~p~n",[Val])
        %% ,
    %% io:format("~p~n",[Term])

    .
