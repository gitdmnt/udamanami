use sea_orm_migration::prelude::*;

pub struct Migration;

impl MigrationName for Migration {
    fn name(&self) -> &str {
        "m20250526_000002_room_pointer"
    }
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(User::Table)
                    .add_column(ColumnDef::new(User::RoomPointer).big_integer().to_owned())
                    .to_owned(),
            )
            .await
    }

    // Define how to rollback this migration: Drop the Bakery table.
    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(User::Table)
                    .drop_column(User::RoomPointer)
                    .to_owned(),
            )
            .await
    }
}

#[allow(clippy::enum_variant_names, dead_code)]
#[derive(Iden)]
pub enum Message {
    Table,
    MessageId,
    ChannelId,
    UserId,
    Timestamp,
    Content,
}

#[allow(clippy::enum_variant_names, dead_code)]
#[derive(Iden)]
pub enum Channel {
    Table,
    ChannelId,
    IsThread,
    Name,
}

#[allow(clippy::enum_variant_names, dead_code)]
#[derive(Iden)]
pub enum User {
    Table,
    UserId,
    Username,
    RoomPointer,
}

#[allow(clippy::enum_variant_names, dead_code)]
#[derive(Iden)]
pub enum CalcVar {
    Table,
    VarName,
    VarValue,
    UserId,
}
