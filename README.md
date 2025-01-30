# Draw calculator - TCG 用ドロー確率計算機
https://acple.github.io/tcg-calculator/

## 使い方
計算に必要なデッキの情報を入力して、条件を指定するだけです。あとは条件などを変更するたびに勝手に再計算されます。

* [見ればだいたいわかるやつ](https://acple.github.io/tcg-calculator/#{"condition":[{"conditions":[{"cards":[0,1],"count":1,"disabled":false,"mode":"AtLeast"}],"disabled":false},{"conditions":[{"cards":[2],"count":1,"disabled":false,"mode":"AtLeast"},{"cards":[3,4],"count":1,"disabled":false,"mode":"AtLeast"}],"disabled":false},{"conditions":[{"cards":[5,6,7],"count":2,"disabled":false,"mode":"Choice"}],"disabled":false},{"conditions":[{"cards":[8],"count":1,"disabled":false,"mode":"AtLeast"},{"cards":[9],"count":1,"disabled":false,"mode":"Remains"}],"disabled":false}],"deck":{"cards":[{"count":3,"name":"一枚初動その1"},{"count":3,"name":"一枚初動その2"},{"count":3,"name":"二枚初動の一枚目"},{"count":3,"name":"二枚初動の二枚目"},{"count":2,"name":"二枚初動の二枚目%20(サブ)"},{"count":3,"name":"三種類から二種類引ければ初動1"},{"count":2,"name":"三種類から二種類引ければ初動2"},{"count":3,"name":"三種類から二種類引ければ初動3"},{"count":3,"name":"一枚初動だけどパーツ引いちゃだめなやつ"},{"count":1,"name":"一枚初動のとき引いちゃいけないパーツ"}],"hand":5,"others":14}})
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
