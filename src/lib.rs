mod builder;
pub mod internal;

/// Re-Exports
pub use smartql_macro::{smartql_object, smartql_init, smartql_init_lazy, SmartQlObject};
pub use crate::builder::QueryBuilder;

use sqlx::{Encode, Database, Type};
use async_trait::async_trait;

#[async_trait]
pub trait SmartQlObject {

    async fn load<'l, DB: Database, PK: Send + Sync + Encode<'l, DB> + Type<DB>,
        PKC: IntoIterator<Item=PK>>(executor: &sqlx::Pool<DB>, keys: PKC) -> sqlx::Result<Self>
        where Self: Sized;

/*    fn query<B: QueryBuilder<<Self as SmartQlObject>::SelfType>>() -> B;

    async fn save(&mut self) -> sqlx::Result<()>;*/
}

/*pub struct X {}

impl SmartQlObject for X {
    async fn load<
        'e, 'q, 'c, DB: Database, E: 'e + Executor<'c, Database=DB>,
        PK: Send + Encode<'q, DB> + Type<DB>, PKC: IntoIterator<Item=PK>
    >(executor: &E, keys: PKC) -> sqlx::Result<Self> {
        let iter = keys.into_iter();
        sqlx::query("SELECT * FROM table WHERE `key` = ?")
            .bind(iter.nth(0).expect("PrimaryKey not set"))
            .fetch_one(executor)
    }

    fn query<B: QueryBuilder<Self>>() -> B {
        unimplemented!()
    }

    async fn save(&mut self) {
        unimplemented!()
    }
}*/