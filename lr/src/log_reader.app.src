{application, log_reader, [
    {description, "read sql from logfile and write to mysql"},
    {vsn, "0.1"},
    {modules, []},
    {mod, {log_reader, []}},
    {registered, []},
    {applications, [kernel, stdlib]},
    {env, [{logdir ,"."},
           {log_file_pattern,"^(.*)-([0-9]+)\\.log$"} %prefix-num.log
          ]}
]}.
