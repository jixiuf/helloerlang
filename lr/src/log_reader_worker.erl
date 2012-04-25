-module(log_reader_worker).
%% API:
-export([start_link/0]).

%% internal export
-export([run/1]).

-define(STATUS_INIT,-1).
-define(STATUS_OK,0).
-define(STATUS_NONE_MATCHED_LOG_FOUND_IN_DIR,1).
-define(STATUS_END_OF_LOG_FILE,2).
-define(STATUS_END_OF_LOG_FILE_BUT_NONE_END_FLAG,3).
-define(STATUS_NO_MORE_LOG,4).
-define(STATUS_FOUNT_NEXT,5).
-define(STATUS_TRYING_FIND_NEXT_LOG,6).
-define(WAITING_TIMEOUT,3000).
-define(END_OF_SQL_FILE_FLAG,"--end_of_sql_file__do_NOT_delete_this_line").

-record(state,{logdir=".",
               previous_log_file=undefined,
               current_log_file=undefined,
               log_file_pattern,
               status=?STATUS_INIT,
               status_file,
               status_filehandle,
               filehandle=undefined,
               position=0,
               buffer=""
              }).
%% {state,".",undefined,"a-2.log",undefined,3,<0.64.0>,1}
start_link()->
    LogDir = log_reader:get_current_app_env(logdir),
    LogFilePattern=log_reader:get_current_app_env(log_file_pattern),
    StatusFile= filename:basename(filename:dirname(filename:absname(LogDir)))++".status",
    catch case filelib:is_regular(StatusFile) of
              true->
                  %% io:format("~p~n",[]),
                  case file:open(filename:join(filename:dirname(filename:absname(LogDir)),StatusFile),[write,read]) of
                      {ok,StatusFileH}->
                          {ok,FileName0}=file:read_line(StatusFileH),
                          {ok,Pos0}=file:read_line(StatusFileH),
                          FileName= string:left(FileName0,length(FileName0)-1),
                          Pos=list_to_integer(string:left(Pos0,length(Pos0)-1)),
                          %% %% FIXME:read buffer ,may be not only one line
                          %% {ok,Buf}=file:read_line(StatusFileH), %
                          State = #state{logdir=LogDir,
                                         previous_log_file=undefined,
                                         current_log_file=FileName,
                                         log_file_pattern=LogFilePattern,
                                         status=?STATUS_FOUNT_NEXT,
                                         status_file=StatusFile,
                                         status_filehandle=StatusFileH,
                                         position=Pos,
                                         buffer=""
                                        },
                          spawn(?MODULE,run,[State]);
                      _ ->
                          undefined
                  end
                      ;
              false ->
                  case file:open(filename:join(LogDir,StatusFile),[write,read]) of
                      {ok,StatusFileH}->
                          CurrentLogFile=fileutil:find_next_log_file(undefined),
                          Status= case CurrentLogFile  of
                                      undefined->
                                          ?STATUS_NONE_MATCHED_LOG_FOUND_IN_DIR;
                                      _->
                                          ?STATUS_FOUNT_NEXT
                                  end,

                          State = #state{logdir=LogDir,
                                         previous_log_file=undefined,
                                         current_log_file=CurrentLogFile,
                                         log_file_pattern=LogFilePattern,
                                         status=Status,
                                         status_file=StatusFile,
                                         status_filehandle=StatusFileH
                                        },
                          spawn(?MODULE,run,[State]);
                      _ ->
                          undefined
                  end
          end
        .

run(#state{current_log_file=CurrentLogFile,
           status=?STATUS_NONE_MATCHED_LOG_FOUND_IN_DIR
          }=State)->
    timer:sleep(?WAITING_TIMEOUT),
    NextLog=fileutil:find_next_log_file(CurrentLogFile), %CurrentLogFile==undefined here
    case NextLog of
        undefined->
            run(State);
        _ ->
            run(State#state{previous_log_file=CurrentLogFile,
                            current_log_file=NextLog,
                            status=?STATUS_FOUNT_NEXT})
    end;
run(#state{previous_log_file=_PreviousLogFile,
           current_log_file=CurrentLogFile,status=?STATUS_FOUNT_NEXT
          }=State) ->
    case file:open(CurrentLogFile,[read]) of
        {ok,FileH}->
            io:format("find next file:~p~n",[CurrentLogFile]) ,
            run(State#state{ status=?STATUS_OK,
                             filehandle=FileH
                           });
        {error, _Reason}->
            %% FIXME:
            io:format("can't read logfile :~p~n",[CurrentLogFile]) ,
            run(State)
    end;
run(#state{current_log_file=CurrentLogFile,
           status=?STATUS_OK,
           filehandle=FileH,
           position=Pos,
           buffer=Buf,
           status_filehandle=StatusFileH
          }=State) ->
    {Line,NewPos,NewBuf}=line_util:read_line(FileH,Pos,Buf),
    case Line of
        eof->
            write_status(CurrentLogFile, NewPos, NewBuf, StatusFileH),
            run(State#state{status=?STATUS_END_OF_LOG_FILE_BUT_NONE_END_FLAG,
                            position=NewPos, buffer=NewBuf});
        error ->
            io:format("error when get_line from file~p~n",[CurrentLogFile]);
        %% FIXME:
        %% run(State)
        Line ->
            case Line of
                ?END_OF_SQL_FILE_FLAG->
                    write_status(CurrentLogFile, Pos, Buf, StatusFileH),
                    run(State#state{status=?STATUS_END_OF_LOG_FILE});
                _ ->
                    log_execute:execute(Line),
                    write_status(CurrentLogFile, Pos, Buf, StatusFileH),
                    run(State#state{position=NewPos,buffer=NewBuf})
            end

    end;
run(#state{status=?STATUS_END_OF_LOG_FILE_BUT_NONE_END_FLAG,
           position=_Pos
          }=State) ->
    %% TODO:reload file
    timer:sleep(5000),
    run(State#state{status=?STATUS_OK});
run(#state{status=?STATUS_END_OF_LOG_FILE, filehandle=FileH }=State) ->
    catch file:close(FileH),                    %maybe it is already closed
    run(State#state{status=?STATUS_TRYING_FIND_NEXT_LOG})
        ;
run(#state{current_log_file=CurrentLogFile,
           status=?STATUS_TRYING_FIND_NEXT_LOG
          }=State) ->
    NextLog=fileutil:find_next_log_file(CurrentLogFile),
    case NextLog of
        undefined->
            run(State#state{status=?STATUS_NO_MORE_LOG});
        _ ->
            run(State#state{previous_log_file=CurrentLogFile,
                            current_log_file=NextLog,
                            status=?STATUS_FOUNT_NEXT,
                            position=0,
                            buffer=""})
    end;

run(#state{status=?STATUS_NO_MORE_LOG }=State) ->
    timer:sleep(?WAITING_TIMEOUT),
    run(State#state{status=?STATUS_TRYING_FIND_NEXT_LOG})
        .

write_status(CurrentLogFile, Pos, Buf, StatusFileH)->
                    file:position(StatusFileH,0),
                    file:write(StatusFileH,CurrentLogFile++"\n"),
                    file:write(StatusFileH,integer_to_list(Pos-length(Buf))++"\n")

    .
