pub mod internal;

/// Re-Exports
pub use smartql_macro::{args, smartql_init, smartql_init_lazy, smartql_object, SmartQlObject};
pub use async_trait::async_trait;

#[async_trait]
pub trait SmartQlObject {
    async fn load(
        executor: &sqlx::Pool<sqlx::MySql>,
        args: sqlx::mysql::MySqlArguments,
    ) -> sqlx::Result<Option<Self>>
    where
        Self: Sized;

    async fn save_all(&mut self, executor: &sqlx::Pool<sqlx::MySql>) -> sqlx::Result<bool>;

    async fn upsert(&mut self, executor: &sqlx::Pool<sqlx::MySql>) -> sqlx::Result<bool>;
}
