use std::time::Duration;

use serenity::{
    builder::{
        CreateButton, CreateEmbed, CreateInteractionResponse, CreateInteractionResponseMessage,
        CreateMessage, EditMessage,
    },
    futures::StreamExt as _,
    model::{
        id::{ChannelId, UserId},
        mention::Mention,
    },
    prelude::*,
};
use strum::{Display, EnumString};

#[derive(Debug, Clone, Copy)]
struct Player {
    id: UserId,
    mana: u8,
    action: Option<Action>,
}

#[derive(Debug)]
enum Turn {
    Settled(UserId),
    Continue,
}

#[derive(Debug, Clone, Copy)]
struct Game {
    player1: Player,
    player2: Player,
    turn: u8,
}

#[derive(Debug)]
enum ActionError {
    UnknownUser,
    NotEnoughCost,
}

impl From<Game> for CreateEmbed {
    fn from(game: Game) -> Self {
        if let Some((player1_action, player2_action)) = game.selected_actions() {
            Self::default()
                .title(format!("ターン{}", game.turn))
                .field("player 1", Mention::from(game.player1.id).to_string(), true)
                .field("マナ", format!("`{}`", game.player1.mana), true)
                .field("技", player1_action.to_string(), true)
                .field("player 2", Mention::from(game.player2.id).to_string(), true)
                .field("マナ", format!("`{}`", game.player2.mana), true)
                .field("技", player2_action.to_string(), true)
        } else {
            Self::default()
                .title(format!("ターン{}", game.turn))
                .field("player 1", Mention::from(game.player1.id).to_string(), true)
                .field("マナ", format!("`{}`", game.player1.mana), true)
                .field("\u{200B}", "\u{200B}", true)
                .field("player 2", Mention::from(game.player2.id).to_string(), true)
                .field("マナ", format!("`{}`", game.player2.mana), true)
        }
    }
}

impl Game {
    const fn new(users: (UserId, UserId)) -> Self {
        Self {
            player1: Player {
                id: users.0,
                mana: 0,
                action: None,
            },
            player2: Player {
                id: users.1,
                mana: 0,
                action: None,
            },
            turn: 1,
        }
    }

    fn action(&mut self, user_id: UserId, action: Action) -> Result<(), ActionError> {
        let player = if user_id == self.player1.id {
            &mut self.player1
        } else if user_id == self.player2.id {
            &mut self.player2
        } else {
            return Err(ActionError::UnknownUser);
        };
        player.action = match action {
            Action::Charge => {
                player.mana += 1;
                Some(action)
            }
            other => {
                if player.mana >= other.cost() {
                    player.mana -= other.cost();
                    Some(other)
                } else {
                    return Err(ActionError::NotEnoughCost);
                }
            }
        };
        Ok(())
    }

    const fn selected_actions(&self) -> Option<(&Action, &Action)> {
        match (&self.player1.action, &self.player2.action) {
            (Some(a), Some(b)) => Some((a, b)),
            _ => None,
        }
    }

    fn next_turn(&mut self) {
        self.turn += 1;
        self.player1.action = None;
        self.player2.action = None;
    }
}

#[derive(Debug, Clone, Copy, Display, EnumString)]
enum Action {
    #[strum(serialize = "チャージ")]
    Charge,
    #[strum(serialize = "バリア")]
    Barrier,
    #[strum(serialize = "ボンバー")]
    Bomber,
    #[strum(serialize = "ソード")]
    Sword,
    #[strum(serialize = "メテオ")]
    Meteor,
}

impl From<Action> for CreateButton {
    fn from(action: Action) -> Self {
        Self::new(action.to_string()).label(format!("{} ({})", action, action.cost()))
    }
}

impl Action {
    const fn cost(&self) -> u8 {
        match self {
            Self::Charge | Self::Barrier => 0,
            Self::Bomber => 1,
            Self::Sword => 2,
            Self::Meteor => 5,
        }
    }
}

const fn fight(a: (UserId, &Action), b: (UserId, &Action)) -> Turn {
    match (a.1, b.1) {
        (&Action::Charge | &Action::Barrier, &Action::Charge | &Action::Barrier) => Turn::Continue,
        // attacking action ("bomber" or "sword") win against "charge"
        (&Action::Bomber | &Action::Sword, &Action::Charge) => Turn::Settled(a.0),
        (&Action::Charge, &Action::Bomber | &Action::Sword) => Turn::Settled(b.0),
        // same kind of attacking action cancel each other out
        (&Action::Bomber, &Action::Bomber)
        | (&Action::Sword, &Action::Sword)
        | (&Action::Meteor, &Action::Meteor) => Turn::Continue,
        // "barrier" prevent from "bomber"
        (&Action::Bomber, &Action::Barrier) | (&Action::Barrier, &Action::Bomber) => Turn::Continue,
        // "sword" break "barrier"
        (&Action::Sword, &Action::Barrier) => Turn::Settled(a.0),
        (&Action::Barrier, &Action::Sword) => Turn::Settled(b.0),
        // "bomber" win against "sword"
        (&Action::Bomber, &Action::Sword) => Turn::Settled(a.0),
        (&Action::Sword, &Action::Bomber) => Turn::Settled(b.0),
        // "meteor" win against any other actions
        (&Action::Meteor, _) => Turn::Settled(a.0),
        (_, &Action::Meteor) => Turn::Settled(b.0),
    }
}

pub async fn cclemon(reply: &ChannelId, ctx: &Context, users: (UserId, UserId)) {
    reply
        .send_message(&ctx, CreateMessage::new().content("CCレモンを始めるよ"))
        .await
        .unwrap();
    let mut game = Game::new(users);
    loop {
        let mut m_embed = reply
            .send_message(&ctx, CreateMessage::new().embed(game.into()))
            .await
            .unwrap();
        let m = reply
            .send_message(
                &ctx,
                CreateMessage::new()
                    .content("技を選んでね")
                    .button(Action::Charge.into())
                    .button(Action::Barrier.into())
                    .button(Action::Bomber.into())
                    .button(Action::Sword.into())
                    .button(Action::Meteor.into()),
            )
            .await
            .unwrap();

        let mut interaction_stream = m
            .await_component_interaction(&ctx.shard)
            .timeout(Duration::from_secs(15))
            .stream();
        while let Some(interaction) = interaction_stream.next().await {
            let user_id = interaction.user.id;
            let action = interaction.data.custom_id.parse().unwrap();
            let content = match game.action(user_id, action) {
                Ok(()) => format!("{}を選択したよ", action),
                Err(ActionError::UnknownUser) => "参加してないでしょ".to_owned(),
                Err(ActionError::NotEnoughCost) => "コストが足りないよ".to_owned(),
            };
            interaction
                .create_response(
                    &ctx,
                    CreateInteractionResponse::Message(
                        CreateInteractionResponseMessage::new()
                            .ephemeral(true)
                            .content(content),
                    ),
                )
                .await
                .unwrap();
            if game.selected_actions().is_some() {
                break;
            }
        }
        m.delete(&ctx).await.unwrap();

        if let Some((a, b)) = game.selected_actions() {
            m_embed
                .edit(&ctx, EditMessage::new().embed(game.into()))
                .await
                .unwrap();
            match fight((game.player1.id, a), (game.player2.id, b)) {
                Turn::Settled(user) => {
                    let content = format!("{}の勝ち", Mention::from(user));
                    reply
                        .send_message(&ctx, CreateMessage::new().content(content))
                        .await
                        .unwrap();
                    break;
                }
                Turn::Continue => {
                    game.next_turn();
                    continue;
                }
            }
        } else {
            let content = match (game.player1.action, game.player2.action) {
                (None, None) => "時間切れで引き分け".into(),
                (Some(_), None) => format!("時間切れで{}の勝ち", Mention::from(game.player1.id)),
                (None, Some(_)) => format!("時間切れで{}の勝ち", Mention::from(game.player2.id)),
                _ => unreachable!(),
            };
            reply
                .send_message(&ctx, CreateMessage::new().content(content))
                .await
                .unwrap();
            break;
        }
    }
}
