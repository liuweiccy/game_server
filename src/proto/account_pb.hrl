-ifndef(PB_PLAYER_PB_H).
-define(PB_PLAYER_PB_H, true).
-record(pb_player, {
    player_id = erlang:error({required, player_id}),
    name = erlang:error({required, name}),
    sex = erlang:error({required, sex}),
    career = erlang:error({required, career})
}).
-endif.

-ifndef(ACCOUNT_LOGIN_TOS_PB_H).
-define(ACCOUNT_LOGIN_TOS_PB_H, true).
-record(account_login_tos, {
    account = erlang:error({required, account})
}).
-endif.

-ifndef(ACCOUNT_LOGIN_TOC_PB_H).
-define(ACCOUNT_LOGIN_TOC_PB_H, true).
-record(account_login_toc, {
    code = erlang:error({required, code}),
    role_list = []
}).
-endif.

-ifndef(CREATE_ROLE_TOS_PB_H).
-define(CREATE_ROLE_TOS_PB_H, true).
-record(create_role_tos, {
    account = erlang:error({required, account}),
    name = erlang:error({required, name}),
    sex = erlang:error({required, sex}),
    career = erlang:error({required, career})
}).
-endif.

-ifndef(CREATE_ROLE_TOC_PB_H).
-define(CREATE_ROLE_TOC_PB_H, true).
-record(create_role_toc, {
    code = erlang:error({required, code}),
    role
}).
-endif.

-ifndef(SELECT_LOGIN_TOS_PB_H).
-define(SELECT_LOGIN_TOS_PB_H, true).
-record(select_login_tos, {
    player_id = erlang:error({required, player_id})
}).
-endif.

