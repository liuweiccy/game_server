%% @author dzlaozhu35@outlook.com
%% @doc 事件监控管理模块
%% 			 1. 存储于玩家进程字典
%%			 2. 存储结构 
%%				 Key 					事件ID
%%				 Val					[{模块1, [Id1, Id2]}, {模块2, [Id1, Id2]}]  (Id 为模块自定义唯一ID,  例如任务模块, ID 表示任务ID)
%%           3. 事件回调函数              handle_event/4  <=====> handle_event(UniqueId, Event, Args, PlayerState)

-module(lib_event_monitor).

-include("common.hrl").

-export([register/3, unregister/3]).

-export([trigger/2, trigger/3]).

-record(event_monitor, {module, monitor_ids = []}).

%% ====================================================================
%% Event API functions
%% ====================================================================

%% @doc 事件触发
%% @param 
%%    Event				事件Id
%%    Args 				参数列表
%%    PlayerState	    玩家状态
%% @end
trigger(Event, Args) ->
	MonitorInfos = get_event_monitor(Event),
	lists:foreach(
	fun(#event_monitor{module = Module, monitor_ids = MonitorIDS}) ->
        broadcast_event(Event, Args, Module, MonitorIDS)
    end, MonitorInfos).

%% @doc 事件触发给指定模块(用于特殊逻辑处理, 提高分发效率)
%% @param 
%%    Module 			模块名
%%    Event				事件Id
%%    Args 				参数列表
%%    PlayerState	    玩家状态
%% @end
trigger(Module, Event, Args) ->
	MonitorInfos = get_event_monitor(Event),
	case lists:keyfind(Module, #event_monitor.module, MonitorInfos) of
		#event_monitor{monitor_ids = MonitorIDS} ->
            broadcast_event(Event, Args, Module, MonitorIDS);
		_ ->
			ignore
	end.

%% @doc 事件监控注册
%% @param 
%%    Event			    事件Id
%%    Module 		    模块名
%%    UniqueId	        唯一ID
%% @end
register(Event, Module, UniqueId) ->
	MonitorInfos = get_event_monitor(Event),
	NewMonitorInfo = 
	case lists:keyfind(Module, #event_monitor.module, MonitorInfos) of
		#event_monitor{monitor_ids = MonitorIDS} = MonitorInfo->
			NewMonitorIDS = util:add_to_set(UniqueId, MonitorIDS),
			MonitorInfo#event_monitor{monitor_ids = NewMonitorIDS};
		_ ->
			#event_monitor{module = Module, monitor_ids = [UniqueId]}
	end,
	NewMonitorInfos = lists:keystore(Module, #event_monitor.module, MonitorInfos, NewMonitorInfo),
	set_event_monitor(Event, NewMonitorInfos).

%% @doc 取消事件注册
%% @param 
%%    Event			    事件Id
%%    Module 		    模块名
%%    UniqueId	        唯一ID
%% @end
unregister(Event, Module, UniqueId) ->
	MonitorInfos = get_event_monitor(Event),
	case lists:keyfind(Module, #event_monitor.module, MonitorInfos) of
		#event_monitor{monitor_ids = MonitorIDS} = MonitorInfo->
			NewMonitorIDS = lists:delete(UniqueId, MonitorIDS),
			NewMonitorInfo = MonitorInfo#event_monitor{monitor_ids = NewMonitorIDS},
			NewMonitorInfos = lists:keystore(Module, #event_monitor.module, MonitorInfos, NewMonitorInfo),
			set_event_monitor(Event, NewMonitorInfos);
		_ ->
			none
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
get_event_monitor(Event) ->
	util:get_dict(Event, []).

set_event_monitor(Event, Val) ->
	util:set_dict(Event, Val).

broadcast_event(Event, Args, Module, MonitorIDS) ->
    lists:foreach(
        fun(UniqueId) ->
            try Module:on_event(UniqueId, Event, Args)
            catch _:Reason -> ?ERROR("event trigger error, Event:~w, Module:~w, UniqueId:~w, Reason:~w",[Event, Module, UniqueId, Reason])
            end
        end, MonitorIDS).