-module(protobuff_routing).

-include("account_pb.hrl").

-export([encode/1, decode/2]).

encode(#account_login_tos{} = Msg) ->
	BinData = account_pb:encode_account_login_tos(Msg),
	<<(erlang:byte_size(BinData) + 32):16, 16#1001:32, BinData/binary>>;
encode(#account_login_toc{} = Msg) ->
	BinData = account_pb:encode_account_login_toc(Msg),
	<<(erlang:byte_size(BinData) + 32):16, 16#1002:32, BinData/binary>>;
encode(#create_role_tos{} = Msg) ->
	BinData = account_pb:encode_create_role_tos(Msg),
	<<(erlang:byte_size(BinData) + 32):16, 16#1003:32, BinData/binary>>;
encode(#create_role_toc{} = Msg) ->
	BinData = account_pb:encode_create_role_toc(Msg),
	<<(erlang:byte_size(BinData) + 32):16, 16#1004:32, BinData/binary>>;
encode(#select_login_tos{} = Msg) ->
	BinData = account_pb:encode_select_login_tos(Msg),
	<<(erlang:byte_size(BinData) + 32):16, 16#1005:32, BinData/binary>>;
encode(Msg) ->
	erlang:error({invalid_proto_msg, Msg}).

decode(16#1001, Bin) -> { handle_account, account_pb:decode_account_login_tos(Bin)};
decode(16#1002, Bin) -> { handle_account, account_pb:decode_account_login_toc(Bin)};
decode(16#1003, Bin) -> { handle_account, account_pb:decode_create_role_tos(Bin)};
decode(16#1004, Bin) -> { handle_account, account_pb:decode_create_role_toc(Bin)};
decode(16#1005, Bin) -> { handle_account, account_pb:decode_select_login_tos(Bin)};
decode(Id, _) ->
	erlang:error({invalid_proto_id, Id}).
