-file("src/account_pb.erl", 1).

-module(account_pb).

-export([encode_select_login_tos/1,
	 decode_select_login_tos/1,
	 delimited_decode_select_login_tos/1,
	 encode_create_role_toc/1, decode_create_role_toc/1,
	 delimited_decode_create_role_toc/1,
	 encode_create_role_tos/1, decode_create_role_tos/1,
	 delimited_decode_create_role_tos/1,
	 encode_account_login_toc/1, decode_account_login_toc/1,
	 delimited_decode_account_login_toc/1,
	 encode_account_login_tos/1, decode_account_login_tos/1,
	 delimited_decode_account_login_tos/1,
	 encode_pb_player/1, decode_pb_player/1,
	 delimited_decode_pb_player/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-record(select_login_tos, {player_id}).

-record(create_role_toc, {code, role}).

-record(create_role_tos, {account, name, sex, career}).

-record(account_login_toc, {code, role_list}).

-record(account_login_tos, {account}).

-record(pb_player, {player_id, name, sex, career}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_select_login_tos(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_select_login_tos(Record)
    when is_record(Record, select_login_tos) ->
    encode(select_login_tos, Record).

encode_create_role_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_create_role_toc(Record)
    when is_record(Record, create_role_toc) ->
    encode(create_role_toc, Record).

encode_create_role_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_create_role_tos(Record)
    when is_record(Record, create_role_tos) ->
    encode(create_role_tos, Record).

encode_account_login_toc(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_account_login_toc(Record)
    when is_record(Record, account_login_toc) ->
    encode(account_login_toc, Record).

encode_account_login_tos(Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode_account_login_tos(Record)
    when is_record(Record, account_login_tos) ->
    encode(account_login_tos, Record).

encode_pb_player(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_pb_player(Record)
    when is_record(Record, pb_player) ->
    encode(pb_player, Record).

encode(pb_player, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(pb_player, Record) ->
    [iolist(pb_player, Record) | encode_extensions(Record)];
encode(account_login_tos, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(account_login_tos, Record) ->
    [iolist(account_login_tos, Record)
     | encode_extensions(Record)];
encode(account_login_toc, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(account_login_toc, Record) ->
    [iolist(account_login_toc, Record)
     | encode_extensions(Record)];
encode(create_role_tos, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(create_role_tos, Record) ->
    [iolist(create_role_tos, Record)
     | encode_extensions(Record)];
encode(create_role_toc, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(create_role_toc, Record) ->
    [iolist(create_role_toc, Record)
     | encode_extensions(Record)];
encode(select_login_tos, Records)
    when is_list(Records) ->
    delimited_encode(Records);
encode(select_login_tos, Record) ->
    [iolist(select_login_tos, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(pb_player, Record) ->
    [pack(1, required,
	  with_default(Record#pb_player.player_id, none), int64,
	  []),
     pack(2, required,
	  with_default(Record#pb_player.name, none), string, []),
     pack(3, required,
	  with_default(Record#pb_player.sex, none), int32, []),
     pack(4, required,
	  with_default(Record#pb_player.career, none), int32,
	  [])];
iolist(account_login_tos, Record) ->
    [pack(1, required,
	  with_default(Record#account_login_tos.account, none),
	  string, [])];
iolist(account_login_toc, Record) ->
    [pack(1, required,
	  with_default(Record#account_login_toc.code, none),
	  int32, []),
     pack(2, repeated,
	  with_default(Record#account_login_toc.role_list, none),
	  pb_player, [])];
iolist(create_role_tos, Record) ->
    [pack(1, required,
	  with_default(Record#create_role_tos.account, none),
	  string, []),
     pack(2, required,
	  with_default(Record#create_role_tos.name, none), string,
	  []),
     pack(3, required,
	  with_default(Record#create_role_tos.sex, none), int32,
	  []),
     pack(4, required,
	  with_default(Record#create_role_tos.career, none),
	  int32, [])];
iolist(create_role_toc, Record) ->
    [pack(1, required,
	  with_default(Record#create_role_toc.code, none), int32,
	  []),
     pack(2, optional,
	  with_default(Record#create_role_toc.role, none),
	  pb_player, [])];
iolist(select_login_tos, Record) ->
    [pack(1, required,
	  with_default(Record#select_login_tos.player_id, none),
	  int64, [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(pikachu, value) -> 1.

int_to_enum(_, Val) -> Val.

decode_select_login_tos(Bytes) when is_binary(Bytes) ->
    decode(select_login_tos, Bytes).

decode_create_role_toc(Bytes) when is_binary(Bytes) ->
    decode(create_role_toc, Bytes).

decode_create_role_tos(Bytes) when is_binary(Bytes) ->
    decode(create_role_tos, Bytes).

decode_account_login_toc(Bytes) when is_binary(Bytes) ->
    decode(account_login_toc, Bytes).

decode_account_login_tos(Bytes) when is_binary(Bytes) ->
    decode(account_login_tos, Bytes).

decode_pb_player(Bytes) when is_binary(Bytes) ->
    decode(pb_player, Bytes).

delimited_decode_pb_player(Bytes) ->
    delimited_decode(pb_player, Bytes).

delimited_decode_account_login_tos(Bytes) ->
    delimited_decode(account_login_tos, Bytes).

delimited_decode_account_login_toc(Bytes) ->
    delimited_decode(account_login_toc, Bytes).

delimited_decode_create_role_tos(Bytes) ->
    delimited_decode(create_role_tos, Bytes).

delimited_decode_create_role_toc(Bytes) ->
    delimited_decode(create_role_toc, Bytes).

delimited_decode_select_login_tos(Bytes) ->
    delimited_decode(select_login_tos, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(pb_player, Bytes) when is_binary(Bytes) ->
    Types = [{4, career, int32, []}, {3, sex, int32, []},
	     {2, name, string, []}, {1, player_id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(pb_player, Decoded);
decode(account_login_tos, Bytes)
    when is_binary(Bytes) ->
    Types = [{1, account, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(account_login_tos, Decoded);
decode(account_login_toc, Bytes)
    when is_binary(Bytes) ->
    Types = [{2, role_list, pb_player,
	      [is_record, repeated]},
	     {1, code, int32, []}],
    Defaults = [{2, role_list, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(account_login_toc, Decoded);
decode(create_role_tos, Bytes) when is_binary(Bytes) ->
    Types = [{4, career, int32, []}, {3, sex, int32, []},
	     {2, name, string, []}, {1, account, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(create_role_tos, Decoded);
decode(create_role_toc, Bytes) when is_binary(Bytes) ->
    Types = [{2, role, pb_player, [is_record]},
	     {1, code, int32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(create_role_toc, Decoded);
decode(select_login_tos, Bytes) when is_binary(Bytes) ->
    Types = [{1, player_id, int64, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(select_login_tos, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(pb_player, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       pb_player),
						   Record, Name, Val)
			  end,
			  #pb_player{}, DecodedTuples),
    Record1;
to_record(account_login_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       account_login_tos),
						   Record, Name, Val)
			  end,
			  #account_login_tos{}, DecodedTuples),
    Record1;
to_record(account_login_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       account_login_toc),
						   Record, Name, Val)
			  end,
			  #account_login_toc{}, DecodedTuples),
    Record1;
to_record(create_role_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       create_role_tos),
						   Record, Name, Val)
			  end,
			  #create_role_tos{}, DecodedTuples),
    Record1;
to_record(create_role_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       create_role_toc),
						   Record, Name, Val)
			  end,
			  #create_role_toc{}, DecodedTuples),
    Record1;
to_record(select_login_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       select_login_tos),
						   Record, Name, Val)
			  end,
			  #select_login_tos{}, DecodedTuples),
    Record1.

decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

