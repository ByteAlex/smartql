mod builder;
pub mod internal;

/// Re-Exports
pub use smartql_macro::{smartql_object, smartql_init, smartql_init_lazy, SmartQlObject};
pub use crate::builder::QueryBuilder;
//use sqlx::{Encode, Database, Type, Executor};

pub trait SmartQlObject {
/*    async fn load<
        'e, 'q, 'c, DB: Database, E: 'e + Executor<'c, Database=DB>,
        PK: Send + Encode<'q, DB> + Type<DB>, PKC: IntoIterator<Item=PK>
    >(executor: &E, keys: PKC) -> sqlx::Result<Self>;

    fn query<B: QueryBuilder<Self>>() -> B;
    async fn save(&mut self) -> sqlx::Result<()>;*/
}

/*pub struct X {}

impl SmartQlObject for X {
    async fn load<
        'e, 'q, 'c, DB: Database, E: 'e + Executor<'c, Database=DB>,
        PK: Send + Encode<'q, DB> + Type<DB>, PKC: IntoIterator<Item=PK>
    >(executor: &E, keys: PKC) -> sqlx::Result<Self> {
        sqlx::query_as("SELECT * FROM table WHERE `key` = ?")
            .bind(keys)
            .fetch_one(executor).await?
    }

    fn query<B: QueryBuilder<Self>>() -> B {
        unimplemented!()
    }

    async fn save(&mut self) {
        unimplemented!()
    }
}*/