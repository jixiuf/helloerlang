-module(line_util).
-export([read_line/3]).
-define(BUFFER_SIZE,4).
%% {Line,NewPos,Buffer}
%% {eof,NewPos,""}
%% {error,OldPos,Buffer}
read_line(FileH,Pos,Buf)->
    case string:chr(Buf,$\n) of
        0->
            case file:pread(FileH,Pos,?BUFFER_SIZE) of
                {ok,Data}->
                    {ok,NewPos}=file:position(FileH,cur),
                    read_line(FileH,NewPos,Buf++Data)
                        ;
                eof ->
                    Line=case Buf of
                             ""->
                                 eof;
                             _->
                                 Buf
                         end,
                    {ok,NewPos}=file:position(FileH,cur),
                    {Line,NewPos,""}
                        ;
                {error,Error} ->
                    io:format("pread error with reason:~p~n",[Error]),
                    {error,Pos,Buf}
            end

                ;
        NewLineIndex->
            Line=string:sub_string(Buf,1,NewLineIndex-1),
            NewLine=case Line of
                        ""->
                            Line;
                        _->                     %length >0 check \r exists ,if exists ,delete it
                            case string:sub_string(Line,NewLineIndex-1) of
                                [$\r]->
                                    string:sub_string(Line,1,NewLineIndex-2)
                                        ;
                                _->
                                    Line
                            end
                    end,
            RestBuf=string:sub_string(Buf,NewLineIndex+1),
            {NewLine,Pos,RestBuf}

    end

        .
