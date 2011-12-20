* 用systool发布
  #+begin_src sh
  cd erlcount_release/        
  erl -env ERL_LIBS  .
  #+end_src

erlcount-1.0.rel
#+begin_src erlang
{release,
 {"erlcount" ,"1.0.0"},
 {erts, "5.8.5"},
 [ {kernel ,"2.14.5"},
   {stdlib,"1.17.5"},
   {ppool,"1.1.0",permanent},
   {erlcount,"1.0.0",transient}
 ]
}.                    %最后一个点不能少。
%% 相应 的版本号可以通过 erl 命令后，会显示当前 Eshell V5.8.5  (abort with ^G)
%% application:which_applications(). 结果是：
%% [{stdlib,"ERTS  CXC 138 10","1.17.5"},
%%  {kernel,"ERTS  CXC 138 10","2.14.5"}]

%% systools:make_script("erlcount-1.0",[local]). 通过此文件生成启动脚本boot

%% the local option means that we want the release to be possible to run from
%% anywhere, and not just the current install.

%% systools:make_tar("erlcount-1.0" ,[{erts, "d:/usr/erl5.8.5/"}]).
%% 生成tar
#+end_src
%% systools:make_script("erlcount-1.0",[local]). 通过 erlcount-1.0.rel  生成启动脚本boot
%% the local option means that we want the release to be possible to run from
%% anywhere, and not just the current install.

   生成的脚本名称如
   erlcount-1.0.script
   erlcount-1.0.boot
   
systools:make_tar("erlcount-1.0" ,[{erts, "d:/usr/erl5.8.5/"}]).
%% 生成 tar.gz
   解压开后的目录如
  drwxrwxrwx  1 jixf       0 12-15 11:06 erts-5.8.5
  drwxrwxrwx  1 jixf       0 12-15 11:06 lib
  drwxrwxrwx  1 jixf       0 12-15 11:06 releases
  ;; 
   
通过 ./erts-5.8.8/bin/erl -boot releases/1.0.0/start可以执行.
可以通过以下方式向 erlcount 传递 directory 参数。
./erts-5.8.4/bin/erl -boot releases/1.0.0/start -erlcount directory '"/home/ferd/code/otp_src_R14B03/"' -noshell
注意单双引号

* 用reltool发布
** 如何发布  
  #+begin_src sh
  cd release
  mkdir rel
  #+end_src
  用到release/erlcount-1.0.config作为配置文件
  #+begin_src erlang
 {ok, Conf} = file:consult("erlcount-1.0.config").
 {ok, Spec} = reltool:get_target_spec(Conf).
 reltool:eval_target_spec(Spec, code:root_dir(), "rel"). % 在rel 目录内生成相
 应的内容
  #+end_src

#+begin_src sh
cd rel
./bin/erl -noshell
./bin/erl -noshell -erlcount directory ' "/home/jixiuf/documents/erlang/erlcount_release/"'
./bin/erl -noshell -erlcount directory ' "/usr/lib/erlang"' max_files '25'
./bin/erl -noshell -erlcount directory ' "/usr/lib/erlang"' max_files '30'
./bin/erl -noshell -erlcount directory ' "/usr/lib/erlang"' max_files '30' 
./bin/erl -noshell -erlcount directory ' "/usr/lib/erlang"' max_files '1' % 只用一个worker 
 
#+end_src

** reltool 使用的配置文件的格式
   其可配置的Options 是分层的，分别是release 层,app层， 及module层
   分别对应 sys ,app ,mode 三个key  ,sys 配置全局属性，app可以覆盖sys 中配置的
   属性，仿佛子承父类，
   {sys, [Options]},            
   {app, Appname [Options]}
   {mod,...}
   
*** {sys,[Options]}
**** erts
     指定 erts application 信息, 具体的字段参考后面介绍的 app 配置.
**** escript
     escript 脚本配置信息, 包含 escript 文件,以及对应的配置信息
**** app
     target system 中包含的 app 信息. 比如所有的 release 都要包含 kernel,
stdlib, 具体的app的相关配置,轻参考后面介绍的 app level 配置

**** mod_cond
设置 module 的包含策略. 此处配置的 mod_cond 是 sys level 的配置, 控制所有的
app 中的 module 包含策略. 当然我们可以对这个信息, 在 app level 进行覆盖. 其包含
多种方式:
    | all     | 表示如果某个 app 被包含,那么其包含的所有模块全部被包含. 假设 app 名为 demoapp, all 不仅包含 demoapp/ebin 下的所有模块, 同时 demoapp.app 中 modules 字段描述的所有模块也将被包括(默认) |
    | ebin    | 表示 ebin 目录下所有的模块,以及相关的模块会被包含                                                                                                                                     |
    | app     | 表示 .app 描述文件,以及相关的模块会被包含                                                                                                                                             |
    | derived | 表示被其他包含的模块用到的相关模块会被包含                                                                                                                                            |
****|incl_cond 设置 applicaion 以及 escript 的包含策略.
其包|3种类型:
    | derived | 表示 包含 没有明确设置 incl_cond, 同时被其他 application 直接或间接引用的application. (默认) |
    | include | 表示 包含 所有没有明确设置 incl_cond 的application                                           |
    | exclude | 表示 排除 排除没有明确设置 incl_cond 的application                                           |
include 和 exclude 意义相反. 一个是包含没有设置 incl_cond 的 apps, 一个是排除没
有设置 incl_cond 的 apps. 一般我们使用默认的 derived 选项,这样保证所有相关的
application 均被包含, 不会出现 target system 中,某个模块没有定义的错误.我们可以
通过前面 reltool GUI 部分介绍的 application 依赖关系图 来了解 application 的交
互相关信息.

**** boot_rel
指定 target system 中默认启动项 (rel), 一个target system 中可能包含很多的
release(通过 rel 来指定)
**** rel
指定 rel 内容 [[http://www.erlang.org/doc/design_principles/release_structure.html#10][(Release specific configuration)]] , 每个 rel 定义会被映射成 rel,
script 和 boot 文件,请参考 Erlang/OTP Design Principles Release
**** relocable
指定 target system 中erl 执行时自动检测路径相关信息({relocable, true}), 还是硬
编码指定路径启动({relocable, false}). 如果 relocable 为 false, 那么 target
system 必须首先通过 reltool:install/2 进行安装, 如果 relocable 为 true, target
system 移动到其他目录时,仍然可以方便的运行. {relocable, true} (默认)
**** profile
     指定 tareget system 的类型, 此选项主要影响:incl_sys_filters,
     excl_sys_filters, incl_app_filters 和 excl_app_filters. 不同的 profile 类
     型, 以上4个 filters 不同.
     主要包含3种:

     | development | 开发测试模式(默认)              |
     | embedded    | 嵌入式完整模式                  |
     | standalone  | 单独模式,不携带相关的可执行文件 |
     在生成 target system 时, 实际上主要有两个步骤:首先创建一个完整的文件候选列
     表,它包含尽可能多的文件; 随后调用各种相关的 filter 定义,对结果进行过滤, 最
     后声称最终的 target system. 一般情况下,我们的target system 要是一个完整的
     可执行的系统,因此我们许要设置 profile 为 embedded. (当然不设置 profile, 只
     是修改4个相关的 filter, 也可以达到我们想要的效果)

**** app_file
     控制 app 的内容, 因为有各种各样的 filter, 因此最终的 target system 中包含
     的模块,可能与 app 文件定义的模块列表不一致, 本选项控制 app 的内容. 主要包
     含3种:
     | keep  | 将 app 直接拷贝到 target system 中(默认)                                                                                                 |
     | strip | 依据 app 文件产生 target system 中的 app 文件, 并将未被包含的模块从 app 文件中移出                                                       |
     | all   | 依据 app 文件产生 tarege system 中的 app 文件, 同时所有的模块将添加到 app 文件中, 如果某个应用没有 app 文件,那么会自动创建一个 app 文件. |


**** debug_info
是否去除 beam 文件中的调试信息: keep 表示保持; strip 表示去除
**** incl_sys_filters
     指定一个正则表达式列表,用来表示哪些系统文件可以出现在 target system 中. 如果某
     个系统文件想被包括, 那么其必须满足 incl_sys_filters 中的某个正则表达式, 同时不
     能满足 excl_sys_filters 中的任何表达式.
     比如:
     {incl_sys_filters,["^bin","^erts","^lib","^releases"]},
     表示 $ERL_ROOT 目录下的 bin, erts, lib, releases 目录均要包含.
     incl_app_filters, excl_app_filters 同 sys 相关的 filters 含义大致相同,只是用来控制 application 的包含规则.
**** excl_sys_filters
     指定一个正则表达式列表,表示哪些系统文件不可以出现在 target system 中. 默认 为
     [].
**** incl_app_filters
     指定一个正则表达式列表,表示 application 中的哪些文件可以被包含. 如果某个文件想
     被包含,至少要满足正则表达式列表中的一个表达式. 默认为 [".*"], 如果设置为 [], 那
     么 application 中的任何文件都不会被包含.
**** excl_app_filters
     指定一个正则表达式列表,表示 application 的哪些文件不可以出现在 target system 中. 默认 为 [].

**** incl_archive_filters
     指定 application 中哪些一级子目录包含在压缩包中(与包含正常的目录对应), 通
     过一个正则表达式列表指定要包含在压缩包中的一级子目录. 默认为 [".*"]
**** excl_archive_filters
     指定一个正则表达式列表,指定 application 的哪些一级目录不包含在压缩包中. 如
     果某个目录,匹配任何一个正则表达式,则不会包含在压缩包中. 默认为
     ["^include$", "^priv$"]
**** archive_opts
     创建压缩包对应的参数,在 zip:create/3 中使用, 请参考 [[http://erlang.org/doc/man/zip.html][zip module]], 默认 为 [].

*** {app,AppName,[Options]}
****    vsn
    指定要包含的 application 的版本, 因为在系统中,可能存在同一应用的多个版本. 如果忽略,则使用最新版本.
****    mod
    模块相关的配置信息. 必须包含一个模块名称,以及其他可选的模块配置(参照后面 mod 配置)
****    mod_cond - 同 sys level 的同名配置含义相同
****    incl_cond - 同上
****    debug_info - 同上
****    incl_app_filters - 同上
****    excl_app_filters - 同上
****    incl_archive_filters - 同上
****    excl_archive_filters - 同上
****    archive_opts - 同上
***   mod 配置
****    incl_cond
        指示模块是否被包含,其覆盖 application 及 system 配置中的 incl_cond 信息.
        其包含3个值:
        include - 表示本模块将被包含
        exclude - 表示本模块不被包含
        derived - 表示如果其他被包含的模块引用本模块,则本模块被也被包含.

****    debug_info

        同 app 配置中 debug_info 描述.



*** demo config
    #+begin_src erlang
{sys,
   [
   {lib_dirs,["/home/jixiuf/erlang"]},
   {boot_rel, "erlips"},
   {rel, "erlips", "0.1", [kernel, stdlib, sasl, mochiweb, erlips]},
   {relocatable, true},
   {profile, embedded},
   {app_file, keep},
   {debug_info, strip},
   {mod_cond, all},
   {incl_cond, derived},

   {incl_app_filters, ["^include", "^priv", "^ebin", "^src"]},
   {excl_app_filters, []},

   {incl_archive_filters,[]},
   {excl_archive_filters,[".*"]},

   {app, kernel, [{incl_cond, include}]},
   {app, stdlib, [{incl_cond, include}]},
   {app, sasl, [{incl_cond, include}]},
   {app, erlips, [{incl_cond, include},
                   {incl_app_filters, [".*"]},
                   {excl_app_filters, ["^log", "^var", "^release"]}]},
   {app, mochiweb, [{incl_cond, include}]},
   {app, runtime_tools, [{incl_cond, include}]}
   ]
}.    
    #+end_src
