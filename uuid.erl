%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%应该是生成 n个长度为m的不重复字符串
%%% @end
%%% Created : 2012-09-20 11:50 by 纪秀峰 <jixiuf@gmail.com>
%%%-------------------------------------------------------------------
-module(uuid).
-export([write_uuid/3,uuid/2]).
%%生成20万16位的uuid写到指定文件中
%% uuid:write_uuid(200000,16,"/tmp/ab").
write_uuid(Count,Len,File)->
    {ok,F}=file:open(File,[write]),
    lists:foreach(fun(UUid)->file:write(F,[UUid,"\n"]) end,uuid(Count,Len)),
    file:close(F)
    .
%% uuid:uuid(3,16)=["uYOukkku8YEuGpN1","8u8uOEkkOEk8GTXR","8Y88ukYOuOEOQTNn"]
uuid(Count,Len)->
	Seed = 25931,
	BaseList = lists:seq(Seed, Seed+Count-1),
    Fun=fun(I)-> gen(I,Len,[])end,
	lists:map(Fun, BaseList).


fbc(N)->
    UniMax = case N of
                 0 -> 6;
                 1 -> 6;
                 2 -> 5;
                 3 -> 5;
                 4 -> 5;
                 5 -> 5;
                 6 -> 5;
                 7 -> 5;
                 8 -> 5;
                 9 -> 5
             end,
    X = random:uniform(UniMax),
    Num = X * 10 + N,
    if (Num >=0) and (Num =< 25 )->
            97 + Num;
       (Num >=26) and (Num =< 51)->
            65 + Num - 26;
       (Num>= 52) and (Num =< 61)->
            48 + Num - 52;
       true
       -> 48
    end
        .

gen(_RandomInt,0,Rel)->
    lists:map(fun(R)-> fbc(R)end,lists:reverse(Rel));
gen(RandomInt,Len,Rel)->
    Id1 = RandomInt div int(Len),
    LI1 = RandomInt rem int(Len),
    gen(LI1,Len-1,[Id1|Rel])
        .
%% uuid:int(10)=10000000000
int(ZeroLen)->
    Str=string:join(["1"|lists:duplicate(ZeroLen,"0")],""),
    list_to_integer(Str).
