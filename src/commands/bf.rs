use std::char;

use serenity::{
    builder::{CreateCommand, CreateCommandOption},
    model::application::{CommandOptionType, ResolvedOption, ResolvedValue},
};

use crate::commands::StManamiSlashCommand;

enum BrainfuckCommand {
    MoveRight,
    MoveLeft,
    Increment,
    Decrement,
    Output,
    Input,
    LoopStart,
    LoopEnd,
    Invalid,
}

impl From<char> for BrainfuckCommand {
    fn from(c: char) -> Self {
        match c {
            '>' => Self::MoveRight,
            '<' => Self::MoveLeft,
            '+' => Self::Increment,
            '-' => Self::Decrement,
            '.' => Self::Output,
            ',' => Self::Input,
            '[' => Self::LoopStart,
            ']' => Self::LoopEnd,
            _ => Self::Invalid,
        }
    }
}

pub const SLASH_BF_COMMAND: StManamiSlashCommand = StManamiSlashCommand {
    name: "bf",
    description: "まなみはいんたぷりた？　なんだよ！",
    register,
    run: |options, _| {
        let result = run(options);
        Box::pin(async move { result })
    },
    is_local_command: false,
};

pub fn register() -> CreateCommand {
    CreateCommand::new("bf")
        .description("Brainfuckを実行するよ")
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "code", "Brainfuckのコード")
                .required(true),
        )
        .add_option(
            CreateCommandOption::new(CommandOptionType::String, "input", "入力する文字列")
                .required(false),
        )
}

pub fn run(option: Vec<ResolvedOption>) -> String {
    let (code, input) = option.iter().fold((None, None), |(code, input), option| {
        match (option.name, &option.value) {
            ("code", ResolvedValue::String(s)) => (Some(*s), input),
            ("input", ResolvedValue::String(s)) => (code, Some(*s)),
            _ => (code, input),
        }
    });

    let Some(code) = code else {
        return "エラーだよ！".to_owned();
    };
    let code = code.chars().map(BrainfuckCommand::from).collect::<Vec<_>>();
    let input = input.unwrap_or("");

    let output = interpreter(code, input);
    match output {
        Ok(output) => {
            if output.is_empty() {
                "出力はなかったよ！".to_owned()
            } else if output.split("\n").count() > 16 {
                let output = output.split("\n").take(16).collect::<Vec<_>>();
                format!(
                    "```\n{}\n...のこり{}行は省略しちゃうね！\n```",
                    &output.join("\n"),
                    output.len() - 16
                )
            } else if output.len() > 1960 {
                format!(
                    "```\n{}\n...のこり{}文字は省略しちゃうね！\n```",
                    &output[..1960],
                    output.len() - 1960
                )
            } else {
                format!("```\n{}\n```", output)
            }
        }
        Err(err) => err,
    }
}

const MAX_LOOP_COUNT: usize = 1024;
const MAX_LOOP_DEPTH: usize = 1024;
const MEMORY_SIZE: usize = 1024;

fn interpreter(code: Vec<BrainfuckCommand>, input: &str) -> Result<String, String> {
    let mut code_pointer = 0;
    let mut memory_pointer = 0;
    let mut memory = vec![0u8; MEMORY_SIZE];
    let mut input_iter = input.chars();
    let mut output = String::new();
    let mut loop_stack = Vec::new();
    let mut loop_count = 0;

    while code_pointer < code.len() {
        let cmd = &code[code_pointer];
        match cmd {
            BrainfuckCommand::MoveRight => memory_pointer += 1,
            BrainfuckCommand::MoveLeft => memory_pointer -= 1,
            BrainfuckCommand::Increment => memory[memory_pointer] += 1,
            BrainfuckCommand::Decrement => memory[memory_pointer] -= 1,
            BrainfuckCommand::Output => output.push(memory[memory_pointer] as char),
            BrainfuckCommand::Input => {
                if let Some(input_char) = input_iter.next() {
                    memory[memory_pointer] = if input_char as u16 > 255 {
                        b'?'
                    } else {
                        input_char as u8
                    };
                } else {
                    memory[memory_pointer] = 0;
                }
            }
            BrainfuckCommand::LoopStart => {
                loop_count += 1;
                if loop_count > MAX_LOOP_COUNT {
                    return Err("ループが多すぎるよ！　無限ループじゃない？".to_owned());
                }

                if memory[memory_pointer] == 0 {
                    let mut loop_depth = 1;

                    while loop_depth > 0 {
                        code_pointer += 1;
                        match code[code_pointer] {
                            BrainfuckCommand::LoopStart => loop_depth += 1,
                            BrainfuckCommand::LoopEnd => loop_depth -= 1,
                            _ => {}
                        }
                        if code_pointer >= code.len() {
                            break;
                        }
                        if loop_depth > MAX_LOOP_DEPTH {
                            return Err("ループが深すぎるよ！".to_owned());
                        }
                    }
                } else {
                    loop_stack.push(code_pointer);
                }
            }
            BrainfuckCommand::LoopEnd => {
                if memory[memory_pointer] != 0 {
                    code_pointer = *loop_stack.last().unwrap() - 1;
                } else {
                    loop_stack.pop();
                    loop_count = 0;
                }
            }
            BrainfuckCommand::Invalid => {}
        }
        code_pointer += 1;
    }
    Ok(output)
}
