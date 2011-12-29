%%    -*- coding:utf-8 -*-
-module(iotest).
-export([write/0,read_line/0,read_all_file/0,write_file/0,read_file_line_by_line/0,consult/0,read/0]).

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
    %% file:consult 用来处理 erlang 格式的 term 数据file:consult会假定file指定
    %% 的文件中包含一串erlang的数据项,如果可以读取文件中的所有数据项,那么返回
    %% {ok,[Term]},否则返回{error,Reason}
    %% {ok,Term} = file:consult("test.dat")
    %% test.dat 中只有一个{key1,value}. 的term ,当然也可以用多个{{},[]}之类
     {ok,[{key1,Val}]} = file:consult("test.dat"),
        io:format("value of key1 is ~p~n",[Val])
        %% ,
    %% io:format("~p~n",[Term])

    .

read()->
    %% io:read  test，注意，不是file:read
    %%io:read 与file:consult 的不同是，前者只读取一个term ,后者全文读出
    {ok,FileH}=file:open("test.dat",read),
    {ok,Term1}=io:read(FileH,''),               %
    io:format("~p~n",[Term1])
        .
