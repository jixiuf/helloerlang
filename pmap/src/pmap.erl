-module(pmap).
-export([pmap/2]).

pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) ->
                             spawn(fun() -> do_fun(S, F, I) end)
                     end, L),
    gather(Pids).

gather([H|T]) ->
    receive
        {H, Result} -> [Result|gather(T)]
    end;
gather([]) ->
    [].

do_fun(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.
