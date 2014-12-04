yaoytoolkit
===========

YAOYToolkit(Yet Another OpenYo Toolkit)はOpenYoをコマンドラインから扱うためのツールです。

#依存
Gauche version 0.9.3.3

#使い方
##初期設定
* endpoint: OpenYo.nna774.net
* username: hoge
* password: pass
* api\_ver: 0.1
で設定する場合の初期設定
```
$ ./yaoy.scm init OpenYo.nna774.net 0.1
$ ./yaoy.scm register hoge pass
```

##コマンド
Yo
fugaさんにYoを送信
```
./yaoy.scm yo fuga
```

YoALL
```
$ ./yaoy.scm yoall
```

Histotyの表示
```
$ ./yaoy.scm history
```

New API Tokenの取得
```
$ ./yaoy.scm token
```
パスワードを求められるので入力する

