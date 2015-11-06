# game_server
Simple Game Server Code

compile with rebar tool

    rebar get-deps compile
  
make proto file

    rebar proto
  
release game server code

    cd rel && rebar generate
  
  
server config file, to config db network param

    config/game.config
  
