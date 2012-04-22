{application, game_log_server, [
                                {description, "emysql frontend for our appliaction "},
                                {vsn, "0.1"},
                                {modules, [game_log_server,emysql_center,emysql_center_sup,emysql_execute,start_app]},
                                {mod, {game_log_server, []}},
                                {registered, []},
                                {applications, [kernel, stdlib, emysql]},
                                {env, [{default_timeout, 5000},
                                       {node_list_info,[{'192.168.1.108',10}]},
                                       {connection_count_each_node,3},
                                       {cookie,"mycookie"},
                                       {mysql_username,"root"},
                                       {mysql_password,"root"},
                                       {mysql_hostname,"192.168.1.108"},
                                       {mysql_port,3306},
                                       {mysql_dbname,"tyLog"},
                                       {mysql_encoding,utf8}
                                      ]}
                               ]}.
                                       %% {pool_id,hello_pool}
