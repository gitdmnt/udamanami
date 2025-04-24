use crate::commands::var;
use crate::commands::CommandContext;
use regex::Regex;

use crate::commands::ManamiPrefixCommand;
use serenity::all::ResolvedOption;

pub struct PrefixCommand;
impl ManamiPrefixCommand for PrefixCommand {
    fn name(&self) -> &'static [&'static str] {
        &["varbulk"]
    }

    fn usage(&self) -> &'static str {
        "!varbulk <codeblock>"
    }

    fn description(&self) -> &'static str {
        ";区切りで複数の変数を一度に定義するよ！"
    }

    async fn run(&self, ctx: &CommandContext<'_>, _: &[ResolvedOption<'_>]) {
        run(ctx).await
    }

    fn is_dm_command(&self) -> bool {
        true
    }

    fn is_guild_command(&self) -> bool {
        true
    }
}

pub async fn run(ctx: &CommandContext<'_>) {
    let input = ctx.args().join(" ");

    let code_pattern = Regex::new(r"```[a-zA-Z0-9]*(.*)```").unwrap();

    //get input in code block
    let input = match code_pattern.captures(&input) {
        Some(caps) => caps.get(1).unwrap().as_str().to_owned(),
        None => return,
    };
    let split: Vec<&str> = input.split(';').collect();

    let mut ctx2 = ctx.clone();

    for s in split {
        if s.trim().is_empty() {
            continue;
        }
        ctx2.command = s.to_owned();
        var::run(&ctx2).await;
    }
}
