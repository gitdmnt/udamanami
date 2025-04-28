use sea_orm_migration::prelude::*;

pub struct Migration;

impl MigrationName for Migration {
    fn name(&self) -> &str {
        "m20250429_000001_create_tables"
    }
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Channel::Table)
                    .col(
                        ColumnDef::new(Channel::ChannelId)
                            .big_integer()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Channel::IsThread).boolean().not_null())
                    .col(ColumnDef::new(Channel::Name).string().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_table(
                Table::create()
                    .table(User::Table)
                    .col(
                        ColumnDef::new(User::UserId)
                            .big_integer()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(User::Username).string().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_table(
                Table::create()
                    .table(Message::Table)
                    .col(
                        ColumnDef::new(Message::MessageId)
                            .big_integer()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Message::ChannelId).big_integer().not_null())
                    .col(ColumnDef::new(Message::UserId).big_integer().not_null())
                    .col(ColumnDef::new(Message::Timestamp).timestamp().not_null())
                    .col(ColumnDef::new(Message::Content).text().not_null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_table(
                Table::create()
                    .table(CalcVar::Table)
                    .col(
                        ColumnDef::new(CalcVar::Id)
                            .integer()
                            .not_null()
                            .auto_increment()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(CalcVar::UserId).big_integer().not_null())
                    .col(ColumnDef::new(CalcVar::VarName).string().not_null())
                    .col(ColumnDef::new(CalcVar::VarValue).string().not_null())
                    .to_owned(),
            )
            .await
    }

    // Define how to rollback this migration: Drop the Bakery table.
    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(CalcVar::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(User::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Channel::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Message::Table).to_owned())
            .await
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(Iden)]
pub enum Message {
    Table,
    MessageId,
    ChannelId,
    UserId,
    Timestamp,
    Content,
}

#[allow(clippy::enum_variant_names)]
#[derive(Iden)]
pub enum Channel {
    Table,
    ChannelId,
    IsThread,
    Name,
}

#[allow(clippy::enum_variant_names)]
#[derive(Iden)]
pub enum User {
    Table,
    UserId,
    Username,
}

#[derive(Iden)]
pub enum CalcVar {
    Table,
    Id,
    UserId,
    VarName,
    VarValue,
}
