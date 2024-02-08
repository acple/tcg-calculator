# Draw calculator - TCG 用ドロー確率計算機
https://acple.github.io/tcg-calculator/

## 使い方
計算に必要なデッキの情報を入力して、条件を指定するだけです。あとは条件などを変更するたびに勝手に再計算されます。

* [見ればだいたいわかるやつ](https://acple.github.io/tcg-calculator/#{"deck":{"others":14,"hand":5,"cards":[{"name":"一枚初動その1","id":"c454a3e6-23e0-499a-9981-bad307c55929","count":3},{"name":"一枚初動その2","id":"aca0c4ba-469f-4a28-aa4f-bf9ee1d13b23","count":3},{"name":"二枚初動の一枚目","id":"adf71bcb-e6de-45b4-9239-01db35e17b6e","count":3},{"name":"二枚初動の二枚目","id":"f06f25f2-802d-4631-ad4f-1ad3e73b3392","count":3},{"name":"二枚初動の二枚目%20(サブ)","id":"34584bb7-72ae-5f92-9def-117fd51163f7","count":2},{"name":"三種類から二種類引ければ初動1","id":"117275a8-07cd-4537-ac14-1661c88bfb4d","count":3},{"name":"三種類から二種類引ければ初動2","id":"fea89b14-b302-4ceb-8aaf-81abed08fe08","count":2},{"name":"三種類から二種類引ければ初動3","id":"dbfbeefe-8ed7-4b5c-b682-4af2aee1b6f2","count":3},{"name":"一枚初動だけどパーツ引いちゃだめなやつ","id":"00a53d8b-cf22-4eb7-a667-3212bcb139a7","count":3},{"name":"一枚初動のとき引いちゃいけないパーツ","id":"059b76c0-891c-4885-8632-2956442a4a58","count":1}]},"conditions":[{"disabled":false,"conditions":[{"mode":"AtLeast","disabled":false,"count":1,"cards":["c454a3e6-23e0-499a-9981-bad307c55929","aca0c4ba-469f-4a28-aa4f-bf9ee1d13b23"]}]},{"disabled":false,"conditions":[{"mode":"AtLeast","disabled":false,"count":1,"cards":["adf71bcb-e6de-45b4-9239-01db35e17b6e"]},{"mode":"AtLeast","disabled":false,"count":1,"cards":["f06f25f2-802d-4631-ad4f-1ad3e73b3392","34584bb7-72ae-5f92-9def-117fd51163f7"]}]},{"disabled":false,"conditions":[{"mode":"Choice","disabled":false,"count":2,"cards":["117275a8-07cd-4537-ac14-1661c88bfb4d","fea89b14-b302-4ceb-8aaf-81abed08fe08","dbfbeefe-8ed7-4b5c-b682-4af2aee1b6f2"]}]},{"disabled":false,"conditions":[{"mode":"AtLeast","disabled":false,"count":1,"cards":["00a53d8b-cf22-4eb7-a667-3212bcb139a7"]},{"mode":"Remains","disabled":false,"count":1,"cards":["059b76c0-891c-4885-8632-2956442a4a58"]}]}]})
* ↑とりあえず自由にいじってみてください
* 条件の箱は中に含まれる条件文を全て満たす確率を表示し、トップの数字は全ての箱のうちどれか一つ以上を満たす確率を表示します
* 条件は追加削除以外にも一部を無効化したり順番を入れ替えたりできます
* Save ボタンを押すと作ったデッキと条件を丸ごとブラウザの URL に書き込むので、そのままブックマークに入れておけばいつでも復元できます
* データはローカルにしか置かないので、入力した情報がインターネットに送信されることはありません

枚数大杉や条件複雑杉だと一生結果が返ってこなかったりエラーになるかもしれません。

## 計算式
* [これ](./src/TcgCalculator.purs)
* 算数とか競プロ得意なひとへ、計算高速化のアイデアください

## いじりたいとき
```console
npm install
npm run build
npm gendevcert # dotnet-cli / あるいは devcert(.pem|.key) の手動生成
npm run server
```
