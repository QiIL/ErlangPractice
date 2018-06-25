(1)
说明:
1. C/S架构, 基于TCP socket通信(gen_tcp)
2. 服务端采用固定port来提供服务
3. 客户端登录时需指定所连接的服务器IP, 并给出自己的昵称, 登录不需要密码
4. 一个客户端发言时, 其他所有客户端看到 "[昵称]发言内容"
5. 有人上线时, 已连接的其他客户端需要看到 "[昵称]上线了" 的提示
6. 有人下线时, 已连接的其他客户端需要看到 "[昵称]下线了" 的提示
7. 对于不同的客户端, 昵称不能重复

(2)
1. 添加账号密码验证, 客户端需要使用合适的账号密码才能登录, 服务端需要知道每个账号正确的密码, 用ETS保存
2. 客户端登录后可以自行修改自己的密码
3. 服务器添加手动踢掉(下线)某个账号的功能
4. 同一个账号多客户端登录时, 后登录的把前面的的挤下线
5. 限制每个客户端发言的频率, 任意一分钟内不可以超过50条
6. 添加私聊功能, 私聊时仅对方看到 "[悄悄话][昵称]内容" 形式的消息