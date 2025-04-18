use crate::commands::CommandContext;
use serenity::{builder::CreateCommand, utils::MessageBuilder};

pub fn register() -> CreateCommand {
    CreateCommand::new("help").description("まなみの自己紹介だよ！")
}

const ABOUT_ME: &str = "# まなみの自己紹介だよ！\n";
const ABOUT_GHOSTWRITE: &str = "## 代筆機能があるよ！\nまなみは代筆ができるよ！　DMに送ってもらったメッセージを`!channel`で指定されたチャンネルに転送するよ！\n";
const ABOUT_AI: &str = "## おはなしもできるよ！\nまなみの部屋でいっぱい話しかけてね！\n";
const ABOUT_SLASH: &str = "## まなみはスラッシュコマンドに対応しているよ！
```
/help                ヘルプを表示するよ！
/ping                起きてたらお返事するね！
/dice                サイコロを振るよ！　ex. 2d6 <= 9
/bf                  まなみはいんたぷりた？　なんだよ！
```
";

const ABOUT_DM: &str = "## まなみはDMでコマンドを受け付けるよ！
```
!channel             代筆先のチャンネルを指定するよ！
!erocheck            あなたがエロガキかどうかを判定するよ！
!help                ヘルプを表示するよ！
!calc <expr>         数式を計算するよ！
!var <name>=<expr>   calcで使える変数を定義するよ！
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ！
!calcsay <expr>      calcの結果を代筆先に送信するよ！
";

const ABOUT_GUILD: &str = "## まなみはグループチャットでコマンドを受け付けるよ！
```
![n]d<m>             m面ダイスをn回振るよ！
!help                ヘルプを表示するよ！
!isprime <n>         nが素数かどうかを判定するよ！
!calc <expr>         数式を計算するよ！
!var <name>=<expr>   calcで使える変数を定義するよ！
!varbulk <codeblock> ;区切りで複数の変数を一度に定義するよ！
!jail <user> [sec]   不届き者を収監して 見せます・袋とじ・管理 以外のカテゴリで喋れなくするよ！
!unjail <user>       収監を解除するよ！
!cclemon <opponent>  CCレモンをするよ！
!clear               コマンドを実行したチャンネルのログを忘れるよ！
```
";

pub fn run() -> String {
    let mut content = MessageBuilder::new();
    content
        .push(ABOUT_ME)
        .push(ABOUT_GHOSTWRITE)
        .push(ABOUT_AI)
        .push(ABOUT_SLASH)
        .push(ABOUT_DM)
        .push(ABOUT_GUILD);
    content.build()
}

pub async fn run_old(ctx: &CommandContext<'_>) {
    let content = run();

    ctx.channel_id.say(ctx.cache_http(), content).await.unwrap();
}
