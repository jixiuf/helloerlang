%%    -*- coding:utf-8 -*-
-module(iotest).
-export([list_dir/0,write/0,read_line/0,read_all_file/0,write_file/0,read_file_line_by_line/0,consult/0,read/0,pread/0,write_consult/0]).
-export([copy/0,fileinfo/0]).

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
    %% io:read(IoDevice,Prompt) -> {ok,Term}|{error,Why}|eof 从IoDevice里面读取
    %% erlang数据项,如果IoDevice指定的是一个已经打开的文件,那么Prompt参数将被忽略,
    %% 如果我们使用io:read来从标准输入读取，那么Prompt只用来提供一个提示符。
    {ok,Term1}=io:read(FileH,''),               %
    file:close(FileH),
    io:format("~p~n",[Term1])
        .

%% 随机读取一个文件：如果读取的文件很大，或者它包含的二进制数据使用了第三方定义
%% 的格式，那么我们可以raw模式下打开文件,然后使用file:pread函数从任何想要访问的
%% 位置读取文件。
%% file:pread(IoDevice,Start,Len)会精确的从IoDevice的第Start个字节开始读，读取长度为Len的字节。
pread()->
    {ok,S} = file:open("test.dat",[read,binary,raw]),
    {ok,Bin}=file:pread(S,22,46),
    io:format("~p~n",[Bin]) ,
    file:close(S)
    .
%% )向一个文件中写入一串erlang数据项term，标准库中没有实现，自己实现： %
write_consult()->
    Write_c=fun(File,Terms)->
              {ok,Fh}= file:open(File,write),
              lists:foreach(fun(Term) -> io:format(Fh,"~p~n",[Term]) end ,Terms),
              file:close(Fh)
      end,
    Write_c("test2.out",[{hello,world},{hello,world2}])
        .

list_dir()->
    {ok,Files_in_Dir}= file:list_dir("."),
    lists:foreach(fun(Dir)->  io:format("~p~n",[Dir])end ,Files_in_Dir )
    .
copy()->
    file:copy("iotest.erl" ,"iotest.erl.bak")
    %% file:delete("iotest.erl.bak"
    %% file:del_dir(dir)
    %% file:
    %% ,
    %% file:copy("./", "./a") % 不能copy dir
    .
fileinfo()->
    %% {ok,Info}= file:read_file_info("iotest.erl"),

    %% filelib模块导出了一些日常使用的函数，如:file_size(File),is_dir(File),这
    %% 些函数只是对read_file_info的封装。
    io:format("~p~n",[ filelib:file_size("iotest.erl")])
        .
