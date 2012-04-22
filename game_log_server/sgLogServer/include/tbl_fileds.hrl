-record(ot_ordforger ,{%
          oT_ACCOUNT_ID                           =0    , % 用户账号
          oT_ACCOUNT_TYPE                         =0    , % 用户账号类型
          oT_USER_ID                              =0    , % 角色ID
          oT_NICKNAME                             =""   , % 角色昵称
          oT_ORDFORGER_TYPE                       =0    , % 装备提升类型
          oT_ORDFORGER_RESULT                     =0    , % 装备提升结果
          oT_ALLOCSLOT_RESULT                     =0    , % 奖励凹槽结果
          oT_EFFECTUP_RESULT                      =0    , % 属性提升结果
          oT_EQUIP_TYPE                           =0    , % 提升的装备类型
          oT_EQUIP_ID                             =0    , % 提升的装备ID
          oT_SOURCE_DATA_INDEX                    =0    , % 提升的装备属性索引
          oT_JEWEL_TYPE                           =0    , % 宝石TYPE
          oT_JEWEL_ID                             =0    , % 宝石ID
          oT_SUCCESSUP_JEWEL1_TYPE                =0    , % 提高成功率宝石类型1
          oT_SUCCESSUP_JEWEL1_ID                  =0    , % 提高成功率防止失败的宝石道具1
          oT_SUCCESSUP_JEWEL2_TYPE                =0    , % 提高成功率宝石类型2
          oT_SUCCESSUP_JEWEL2_ID                  =0    , % 提高成功率防止失败的宝石道具2
          oT_ORDFORGER_TIME                       =0    , % 时间
          oT_ORDFORGER_MAP                        =0    , % 地点
          oT_ORDFORGER_NOTE                       =""   , % 备注
          oT_ISSUERS_ID                           =""   , % 发行方标识
          oT_GAME_SERVER_NAME                     =""   , % 服务器NAME
          oT_GAME_ZONE_NAME                       =""   , % 线NAME
          oT_WRITE_TIME                           =0     % 日志写入日期
         }).

-record(ot_recharginglog ,{%
          oT_ACCOUNT_ID                           =0    , % 用户账号
          oT_ACCOUNT_TYPE                         =0    , % 用户账号类型
          oT_USERID                               =0    , % 角色ID
          oT_NICKNAME                             =""   , % 角色昵称
          oT_RECHARGING_KIND                      =0    , % 充值类型
          oT_RECHARGING_PRICE                     =0    , % 充值面额
          oT_CHANNEL_EXPLAIN                      =""   , % 充值渠道名
          oT_RECHARGING_CARDCODE                  =""   , % 充值卡号
          oT_RECHARGING_CARDPASS                  =""   , % 充值密码
          oT_RECHARGING_TIME                      =0    , % 充值时间
          oT_ISSUERS_ID                           =""   , % 发行方标识
          oT_GAME_SERVER_NAME                     =""   , % 服务器NAME
          oT_GAME_ZONE_NAME                       =""   , % 线NAME
          oT_WRITE_TIME                           =0     % 日志写入日期
         }).

-record(tbl_add_exp ,{%
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 角色ID
          playerName                              =""   , % 角色昵称
          addExpType                              =0    , % 获得经验的方式
          isLevelUp                               =0    , % 等级是否有变化
          oldLevel                                =0    , % 增加经验以前等级
          newLevel                                =0    , % 增加经验以后等级
          addExp                                  =0    , % 获得经验数
          giverId                                 =0    , % NPCID和掉落经验的怪物ID,GM角色ID
          taskId                                  =0    , % 获取奖励经验的任务和活动ID
          logTime                                 =0    , % 用户获得经验的时间
          mapId                                   =0    , % 用户获得经验的地图
          clientVersion                           =""   , % 客户端版本
          clientType                              =""   , % 客户端平台类型
          issuers                                 =""   , % 发行方标识
          flashPlayerVersion                      =""   , % FlashPlayer版本
          connectType                             =""   , % 连接类型
          gameServerName                          =""   , % 服务器NAME
          ipAddr                                  =""   , % IP地址
          accountId                               =0     % 用户账号
         }).

-record(tbl_add_gold ,{%
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 角色ID
          playerName                              =""   , % 角色昵称
          addGoldType                             =0    , % 获得金币的方式,1表示此为充值
          addMoney                                =0    , % 获得元宝
          addCoin                                 =0    , % 获得金币数
          giverId                                 =0    , % NPCID和掉落金币的怪物ID,GM角色ID
          taskId                                  =0    , % 获取奖励物品的任务和活动ID
          logTime                                 =0    , % 用户获得金币的时间
          mapId                                   =0    , % 用户获得金币的地图
          sellItemType                            =0    , % 所卖物品的类型
          sellItemTid                             =0    , % 所卖物品的ID
          sellItemId                              =0    , % 所卖物品的属性索引
          clientVersion                           =""   , % 客户端版本
          clientType                              =""   , % 客户端平台类型
          issuers                                 =""   , % 发行方标识
          flashPlayerVersion                      =""   , % FlashPlayer版本
          gameServerName                          =""   , % 服务器NAME
          ipAddr                                  =""   , % ip地址
          accountId                               =0     % 用户账号
         }).

-record(tbl_add_item ,{%
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 角色ID
          playerName                              =""   , % 角色昵称
          addItemType                             =0    , % 获得物品的方式
          itemCount                               =0    , % 获得物品的数量
          itemType                                =0    , % 物品的种类
          itemTid                                 =0    , % 物品的模版ID
          itemId                                  =0    , % 物品索引ID
          needCoin                                =0    , % 销售价格(游戏金币)
          needMoney                               =0    , % 销售价格（UU币）
          logTime                                 =0    , % 用户获得物品的时间
          mapId                                   =0    , % 用户获得物品的地图
          giverId                                 =0    , % NPCID和掉落物品的怪物ID,GM角色ID
          taskId                                  =0    , % 获取奖励物品的任务和活动ID
          addItemResult                           =0    , % 获得物品的结果
          clientVersion                           =""   , % 客户端版本
          clientType                              =""   , % 客户端平台类型
          issuers                                 =""   , % 发行方标识
          flashPlayerVersion                      =""   , % FlashPlayer版本
          connectType                             =""   , % 连接类型
          gameServerName                          =""   , % 服务器NAME
          ipAddr                                  =""   , % IP地址
          accountId                               =0     % 用户账号
         }).

-record(tbl_consume_item ,{%
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 角色ID
          playerName                              =""   , % 角色昵称
          consumeItem                             =0    , % 消耗道具的方式
          consumeCount                            =0    , % 消耗的物品的数量
          itemType                                =0    , % 物品类型
          itemTid                                 =0    , % 物品ID
          itemId                                  =0    , % 装备属性索引
          itemSellCoin                            =0    , % 出售物品给NPC获得的金钱
          logTime                                 =0    , % 消耗物品的时间
          mapId                                   =0    , % 消耗物品时所在地图
          sellNpcId                               =0    , % 出售的NPC ID,GM角色ID
          clientVersion                           =""   , % 客户端版本
          clientType                              =""   , % 客户端平台类型
          issuers                                 =""   , % 发行方标识
          flashPlayerVersion                      =""   , % FlashPlayer版本
          connectType                             =""   , % 连接类型
          gameServerName                          =""   , % 服务器NAME
          ipAddr                                  =""   , % IP地址
          accountId                               =0     % 用户账号
         }).

-record(tbl_item_change ,{%
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 角色ID
          playerName                              =""   , % 角色昵称
          itemId                                  =0    , % 佩戴装备类型
          itemTid                                 =0    , % 佩戴装备ID
          itemPosId                               =0    , % 佩戴装备属性索引
          extList                                 =""   , % 被替换的装备类型
          oT_OLDEQUIP_ID                          =0    , % 被替换的装备ID
          oT_OLDEQUIP_SOURCE_DATA_INDEX           =0    , % 被替换的装备属性表FLD_ID
          oT_ACTION_TYPE                          =0    , % 动作类型
          oT_ACTION_TIME                          =0    , % 动作时间
          oT_EQUIPMENT_MAP                        =0    , % 佩戴装备地图
          oT_EQUIPMENT_NOTE                       =""   , % 备注
          oT_ISSUERS_ID                           =""   , % 发行方标识
          oT_GAME_SERVER_NAME                     =""   , % 服务器NAME
          oT_GAME_ZONE_NAME                       =""   , % 线NAME
          account                                 =0     % 用户账号
         }).

-record(tbl_on_off ,{%
          accountId                               =0    , % 用户账号
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 玩家ID
          headHeroId                              =0    , % 主角ID
          playerLevel                             =0    , % 主角等级
          playerName                              =""   , % 角色昵称
          logTime                                 =0    , % 上线下线时间
          clientVersion                           =""   , % 客户端版本
          clientType                              =""   , % 客户端平台类型
          issuers                                 =""   , % 发行方标识
          flashPlayerVersion                      =""   , % FlashPlayer版本
          connectType                             =""   , % 连接类型
          gameServerName                          =""   , % 服务器NAME
          ipAddr                                  =""   , % IP地址
          onOffType                               =0     % 上线下线类型（1=登入，2=创角，3=角色登入，4=退出游戏）
         }).

-record(tbl_onlinenum ,{%
          onlinenum                               =0    , % 在线人数
          gameServerName                          =""   , % 服务器名字
          logTime                                 =0     % 记录时间
         }).

-record(tbl_reg ,{%
          accountType                             =0    , % 用户账号类型
          playerId                                =0    , % 角色ID
          playerName                              =""   , % 角色昵称
          logTime                                 =0    , % 注册时间
          clientVersion                           =""   , % 客户端版本
          clientType                              =""   , % 客户端平台类型
          issuers                                 =""   , % 发行方标识
          flashPlayerVersion                      =""   , % FlashPlayer版本
          connectType                             =""   , % 连接类型
          gameServerName                          =""   , % 服务器NAME
          ipAddr                                  =""   , % IP地址
          accountId                               =0     % 用户账号
         }).
