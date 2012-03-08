-module(test).
-export([main/0]).

main()->
    Dog = animal:new(dog),
    Dog:say(),

    Cat  = animal:new(cat),
    Cat:say()
        .
