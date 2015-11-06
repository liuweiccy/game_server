%%%-------------------------------------------------------------------
%%% @author dzlaozhu35@outlook.com
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% 日志模块
%%% @end
%%% Created : 05. 十一月 2015 10:16
%%%-------------------------------------------------------------------
-module(logger).

%% API
-export([start/0, stop/0, set_level/1]).

start() ->
    {ok, LogPath} = application:get_env(log_path),
    {ok, LogLevel} = application:get_env(log_level),
    error_logger:tty(false), % 关闭系统自带tty
    gen_event:add_handler(error_logger, logger_file_h, [LogPath]),
    gen_event:add_handler(error_logger, logger_tty_h, []),
    set_level(LogLevel),
    ok.

stop() ->
    error_logger:delete_report_handler(logger_file_h),
    error_logger:delete_report_handler(logger_tty_h),
    error_logger:tty(true).


set_level(LogLevel) ->
    loglevel:set(LogLevel).
