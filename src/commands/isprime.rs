use crate::commands::CommandContext;

pub async fn run(ctx: &CommandContext<'_>) {
    let command_args = ctx.args();

    if command_args.len() != 1 {
        ctx.channel_id
            .say(ctx.cache_http(), "使い方: `!isprime <number>`")
            .await
            .unwrap();
        return;
    };

    let Ok(num) = command_args[0].parse::<u64>() else {
        ctx.channel_id
            .say(ctx.cache_http(), "わかんないよ")
            .await
            .unwrap();
        return;
    };

    let (is_prime, factor) = match num {
        0 | 1 => (false, vec![]),
        2 => (true, vec![2]),
        _ => {
            let mut num = num;
            let mut factor = vec![];

            while num % 2 == 0 {
                num /= 2;
                factor.push(2);
            }

            let mut i = 3;

            while i * i <= num {
                if num % i == 0 {
                    num /= i;
                    factor.push(i);
                } else {
                    i += 2;
                }
            }

            if num != 1 {
                factor.push(num);
            }

            if factor.len() == 1 {
                (true, factor)
            } else {
                (false, factor)
            }
        }
    };

    let is_prime = format!(
        "{}は{}",
        num,
        if is_prime {
            "素数だよ".to_owned()
        } else if factor.is_empty() {
            "素数じゃないよ。あたりまえでしょ？".to_owned()
        } else {
            format!("素数じゃないよ。素因数は{:?}だよ", factor)
        }
    );
    ctx.channel_id
        .say(ctx.cache_http(), is_prime)
        .await
        .unwrap();
}
