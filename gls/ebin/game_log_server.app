{application, game_log_server, [
                                {description, "emysql frontend for our appliaction "},
                                {vsn, "0.1"},
                                {modules, [game_log_server,emysql_center,emysql_center_sup]},
                                {mod, {game_log_server, []}},
                                {registered, []},
                                {applications, [kernel, stdlib, emysql]},
                                {env, [{default_timeout, 5000},
                                       {node_list_info,[{'192.168.0.44',3}]},
                                       {connection_count_each_node,3},
                                       {cookie,"DJQWUOCYZCIZNETCXWES"},
                                       {mysql_username,"tyLog"},
                                       {mysql_password,"2104"},
                                       {mysql_hostname,"192.168.0.200"},
                                       {mysql_port,3306},
                                       {mysql_dbname,"tyLog"},
                                       {mysql_encoding,utf8}
                                      ]}
                               ]}.
                                       %% {pool_id,hello_pool}
