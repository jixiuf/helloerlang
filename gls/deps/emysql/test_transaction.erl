% Use: erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell

-module(test_transaction).
-export([run/0]).

run() ->

    crypto:start(),
    application:start(emysql),

    emysql:add_pool(hello_pool, 1,
        "root", undefined, "192.168.1.102", 3306,
        "cheetah", utf8),

    emysql:execute(hello_pool, <<"DELETE FROM investors where username = 'slepher'">>, []),

    {an_exception, {}} = 
        try
            emysql:transaction(
              hello_pool,
              fun(Connection) ->
                      emysql_conn:execute(Connection,
                                          <<"INSERT INTO investors set username = 'slepher'">>, []),
                      exit(an_exception)
              end)
        catch
            _:Exception ->
                Exception
        end,
    
    {aborted, giveup} = 
        emysql:transaction(
          hello_pool,
          fun(Connection) ->
                  emysql_conn:execute(Connection, <<"INSERT INTO investors set username = 'slepher'">>, []),
                  emysql:abort(giveup)
          end),

    Result = emysql:execute(hello_pool, <<"SELECT id from investors where username = 'slepher'">>),
    {result_packet, _, _, [],<<>>} = Result,
    Result2 = 
        emysql:transaction(
          hello_pool,
          fun(Connection) ->
                  emysql_conn:execute(Connection, <<"INSERT INTO investors set username = 'slepher'">>, []),
                  emysql_conn:execute(Connection, <<"SELECT LAST_INSERT_ID()">>, [])
          end),
    {atomic, {result_packet, _, _, [[Val]],<<>>}} = Result2,
    error_logger:info_msg("[~p] val is ~p", [?MODULE, Val]).
