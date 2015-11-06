%% =====================================================
%% <data_map_type> 数据文件,请勿编辑
%% =====================================================
-module(data_map_type).

-include("data_record.hrl").

-export([get_ids/0, get/1]).

get_ids() ->
	[1, 2].

get(1) ->
	[
	11,
	14
	];

get(2) ->
	[
	12,
	13
	];

get(_Other) ->
	undefined.
