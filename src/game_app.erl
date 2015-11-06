-module(game_app).

-behaviour(application).

-include("common.hrl").


%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, SupPid} = game_sup:start_link(),

    %% 启动日志
    logger:start(),

    %% 创建ETS
    ok = memcache:create_ets(),

    %% 加载DETS
    ok = memcache:create_dets(),

    %% 启动玩家进程监控树
    ok = sup_player:start(SupPid),

    %% 启动地图进程监控数
    ok = sup_map:start(SupPid),

    %% 启动固定地图场景
    ok = lib_map:init(),

    %% 启动网络相关进程和监控树
    networking:start(SupPid),

    ?DEBUG("游戏服务器启动成功"),

    {ok, SupPid}.

stop(_State) ->
    %% 关闭网络连接
    networking:stop(),

    %% 关闭玩家进程（确保保存数据退出）

    %% 关闭dets
    memcache:close_dets(),

    ok.