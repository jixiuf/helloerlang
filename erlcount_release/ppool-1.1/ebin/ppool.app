%% 需要把此文件copy 到ebin/ 文件名以.app 结尾
{application ,ppool,
 [{vsn, "1.1.0"},                                                             %版本号
  {description, "a simple pool "},
  {applications,[stdlib,kernel]},
  {modules,[ppool,ppool_serv,ppool_super,ppool_supersuper,ppool_worker_sup]}, %本 application内有哪些 module
  {registered,[ppool]},                                                        %本 application内有哪些 register
  %%{mod, {CallbackMod, Args}} 回调module, application从何处启动，信息在此给出
  %%This tells OTP that when  starting your application, it should call CallbackMod:start(normal, Args).
  %%and  CallbackMod:stop(Args) for stop
  %%所以  ppool 模块内要有start/2 stop/1
  %% start/2 返回{ok, Pid} or {ok, Pid, SomeState}
  %%stop/1  takes the state returned by start/2 as an argument
  {mod,{ppool,[]}},
  {env,
   [
    {loglevel,info}                           %支持debug ,info 两个值，详见ppool_log.erl
   ]
  }
 ]
}
.
