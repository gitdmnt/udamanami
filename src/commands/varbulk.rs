use crate::commands::var;
use crate::commands::CommandContext;
use regex::Regex;

use crate::commands::ManamiPrefixCommand;

pub const PREFIX_VARBULK_COMMAND: ManamiPrefixCommand = ManamiPrefixCommand {
    name: "varbulk",
    alias: &[],
    usage: "!varbulk <codeblock>",
    description: ";区切りで複数の変数を一度に定義するよ！",
    run: |ctx| Box::pin(run(ctx)),
    is_dm_command: true,
    is_guild_command: true,
};

pub async fn run(ctx: CommandContext<'_>) {
    let input = ctx.args().join(" ");

    let code_pattern = Regex::new(r"```[a-zA-Z0-9]*(.*)```").unwrap();

    //get input in code block
    let input = match code_pattern.captures(&input) {
        Some(caps) => caps.get(1).unwrap().as_str().to_owned(),
        None => return,
    };
    let split: Vec<&str> = input.split(';').collect();

    for s in split {
        if s.trim().is_empty() {
            continue;
        }
        var::var(ctx.channel_id, ctx.cache_http(), s.to_owned(), ctx.bot).await;
    }
}
