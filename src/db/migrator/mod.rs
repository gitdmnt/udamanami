use sea_orm_migration::prelude::*;

mod m20250429_000001_create_tables;
mod m20250526_000002_room_pointer;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20250429_000001_create_tables::Migration),
            Box::new(m20250526_000002_room_pointer::Migration),
        ]
    }
}
