%% =====================================================
%% <data_map> 数据文件,请勿编辑
%% =====================================================
-module(data_map).

-include("data_record.hrl").

-export([get_ids/0, get/1]).

get_ids() ->
	[11, 12, 13, 14].

get(11) ->
	#map_template{
		map_doc_id = 11,
		map_type = 1,
		map_name = <<"细浪">>,
		relive_pos = {15,2,3},
		enter_pos = {12,22}
		};

get(12) ->
	#map_template{
		map_doc_id = 12,
		map_type = 2,
		map_name = <<"巨石">>,
		relive_pos = {11,2,12},
		enter_pos = {12,22}
		};

get(13) ->
	#map_template{
		map_doc_id = 13,
		map_type = 2,
		map_name = <<"神谕">>,
		relive_pos = {11,2,12},
		enter_pos = {12,22}
		};

get(14) ->
	#map_template{
		map_doc_id = 14,
		map_type = 1,
		map_name = <<"天堂">>,
		relive_pos = {11,2,12},
		enter_pos = {12,22}
		};

get(_Other) ->
	undefined.
