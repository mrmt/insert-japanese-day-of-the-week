# insert-japanese-day-of-the-week

日付文字列の直後で実行すると、その日付の曜日表記を日本語で挿入するEmacs Lisp関数です。

[![demonstration video](https://github.com/mrmt/insert-japanese-day-of-the-week/blob/2fb7b3dc1e8844dfd3d274961cdb7050c703af7a/assets/video.jpg?raw=true)](https://www.youtube.com/watch?v=iuNncIsojvc)

## 実行例

このような例文があったとして、

```text
お世話になっております。
次回ミーティングは2025年7月25日 10:00～11:30に実施いたします。
開催予備日は7/30ないしは8.1を予定しております。
資料提出は2025/07/20までにお願いいたします。
```

「★」マークのところで `insert-japanese-day-of-the-week` を実行すると、

```text
お世話になっております。
次回ミーティングは2025年7月25日★ 10:00～11:30に実施いたします。
開催予備日は7/30★ないしは8.1★を予定しております。
資料提出は2025/07/20★までにお願いいたします。
```

以下のように曜日が挿入されます。

```text
お世話になっております。
次回ミーティングは2025年7月25日(金) 10:00～11:30に実施いたします。
開催予備日は7/30(水)ないしは8.1(金)を予定しております。
資料提出は2025/07/20(日)までにお願いいたします。
```

## 使い方

パッケージ対応は(まだ)していませんので、以下の手順で利用してください。

1. `insert-japanese-day-of-the-week.el` を適宜設置し、評価します
2. 適宜キーバインドします。私は `(global-set-key "\C-x\C-d" 'insert-japanese-day-of-the-week)` として `C-x` `C-d` にバインドしています。

## ライセンス

GPL version 3
