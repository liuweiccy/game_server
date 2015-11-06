# game_server
Simple Game Server Code

项目编译

    rebar get-deps compile
  
协议文件生成

    rebar proto
  
发布服务器版本

    cd rel && rebar generate
  
  
服务器配置

    config/game.config
  
启动服务器

    ./rel/game/bin/game console


目录说明：


	1. proto_define   protobuffs协议定义目录
	2. plugins        rebar插件目录，目前只有proto插件生成协议路由文件和protobuffs解析文件
	3. src	          项目源代码
	4. src/data       项目的配置文件目录（由外部工具生成， 代码自动化）
	5. src/proto      protobuffs路由和解析文件
	6. src/sys        系统相关代码，热更新，dbg调试，轻量级日志模块（基于error_logger），动态编译，profile日志
	7. src/handle     客户端协议处理模块
	8. src/map        地图模版逻辑代码实现
	9. src/proc       游戏进程实现
