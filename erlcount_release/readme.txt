* 用systool发布 
cd erlcount_release/        
erl -env ERL_LIBS  .

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
  #+begin_src shell
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

#+begin_src shell
cd rel
./bin -noshell
./bin -noshell -erlcount directory ' "/home/jixiuf/documents/erlang/erlcount_release/"'
#+end_src
