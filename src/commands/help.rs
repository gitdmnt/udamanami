use crate::commands::CommandContext;
use serenity::utils::MessageBuilder;

pub async fn run(ctx: &CommandContext<'_>) {
    // 文章
    let about_me = "# まなみの自己紹介だよ！\n";
    let about_ghostwrite = "## 代筆機能があるよ！\nまなみは代筆ができるよ！　DMに送ってもらったメッセージを`!channel`で指定されたチャンネルに転送するよ！\n";

    let about_dm = "## まなみはDMでコマンドを受け付けるよ！
```
!channel             代筆先のチャンネルについてだよ
!erocheck            あなたがエロガキかどうかを判定するよ
!help                このヘルプを表示するよ
!ping                pong!
!calc <expr>         数式を計算するよ
!var <name>=<expr>   calcで使える変数を定義するよ
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ
!calcsay <expr>      calcの結果を代筆先に送信するよ
```
";

    let about_guild = "## まなみはグループチャットでコマンドを受け付けるよ！
```
![n]d<m>             m面ダイスをn回振るよ
!help                このヘルプを表示するよ
!isprime <n>         nが素数かどうかを判定するよ
!calc <expr>         数式を計算するよ
!var <name>=<expr>   calcで使える変数を定義するよ
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ
!jail <user> [sec]   不届き者を収監して 見せます・袋とじ・管理 以外のカテゴリで喋れなくするよ
!unjail <user>       収監を解除するよ
!cclemon <opponent>  CCレモンをするよ
!clear               コマンドを実行したチャンネルのログを忘れるよ
```
";

    let mut content = MessageBuilder::new();
    content
        .push(about_me)
        .push(about_ghostwrite)
        .push(about_dm)
        .push(about_guild);
    let content = content.build();

    ctx.channel_id.say(&ctx.http_cache, content).await.unwrap();
}
