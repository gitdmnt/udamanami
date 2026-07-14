use crate::commands::ManamiSlashCommand;

use serenity::model::application::ResolvedValue;

pub const SLASH_ISPRIME_COMMAND: ManamiSlashCommand = ManamiSlashCommand {
    name: "isprime",
    usage: "/isprime <n>",
    description: "nが素数かどうかを判定するよ！",
    register,
    run: |options, _| Box::pin(async move { run(options) }),
    is_local_command: false,
};

fn register() -> serenity::builder::CreateCommand {
    serenity::builder::CreateCommand::new("isprime")
        .description("nが素数かどうかを判定するよ！")
        .add_option(
            serenity::builder::CreateCommandOption::new(
                serenity::model::application::CommandOptionType::Integer,
                "n",
                "判定したい数字",
            )
            .min_int_value(0)
            .required(true),
        )
}

fn run(options: Vec<serenity::model::application::ResolvedOption<'_>>) -> String {
    let ResolvedValue::Integer(num) = options[0].value else {
        return "わかんないよ".to_owned();
    };
    let num = num as u64;

    let (is_prime, factor) = check_is_prime(num);

    message(num, is_prime, factor)
}

fn check_is_prime(num: u64) -> (bool, Vec<u64>) {
    match num {
        0 | 1 => (false, vec![]),
        2 => (true, vec![2]),
        _ => {
            let mut num = num;
            let mut factor = vec![];

            while num.is_multiple_of(2) {
                num /= 2;
                factor.push(2);
            }

            let mut i = 3;

            while i * i <= num {
                if num.is_multiple_of(i) {
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
    }
}

fn message(num: u64, is_prime: bool, factor: Vec<u64>) -> String {
    format!(
        "{}は{}",
        num,
        if is_prime {
            "素数だよ！".to_owned()
        } else if factor.is_empty() {
            "素数じゃないよ。あたりまえでしょ？".to_owned()
        } else {
            format!("素数じゃないよ。素因数は{factor:?}だよ。")
        }
    )
}
