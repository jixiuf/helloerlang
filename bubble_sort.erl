 %% -*- coding:utf-8 -*-
-module(bubble_sort).
-export([sort/1,exchange/1]).
%% 冒泡排序
%% 跟冒泡排序的算法不完全一样
%% 比如
%% 1,3,5,2,4,6
%% 进行一次exchange(之后),结果为
%% 6,5,4,2,3,1(最大的数排在最前)
%% 而真正冒泡排序一次交换后的结果为:
%% 1,3,2,4,5,6 (最大的数排在最后)



sort([])->
    [];
sort(L) ->
    sort(L,[])
        .
sort([],Result)->
    Result;
sort(List,Result)->
    [Biggest|Rest]= exchange(List),
    sort(Rest,[Biggest|Result])
    .


%% after exchange ,the first element of list is the biggest
%% 一次exchange后，最大的数为List的第一个元素，下次交换可以排除掉第一个元素,
%% 即依次取出最大的元素,完成排序的过程
exchange(List)->
    exchange(List,[])
    .

exchange([H],List) ->
    [H|List];
exchange([H,H2|T],List)when H>H2 ->
    exchange([H|T],[H2|List]);
exchange([H,H2|T],List)when H=<H2 ->
    exchange([H2|T],[H|List]).
