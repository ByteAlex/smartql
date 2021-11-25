use smartql::{smartql_object, smartql_init, smartql_init_lazy, SmartQlObject, internal::{SmartQlMetaData}};
use async_trait::async_trait;

#[tokio::main]
pub async fn main() {
    let executor = sqlx::MySqlPool::connect(env!("DATABASE_URL"))
        .await.expect("Failed to connect to mysql");

    #[smartql_object]
    struct Table {
        #[smartql(primary, incremental)]
        pub a: u32,
        #[smartql(primary)]
        pub b: u32,
        pub c: String,
    }

    use async_trait::async_trait;
    use sqlx::{Encode, Database, Type};
    #[async_trait]
    impl SmartQlObject for Table
    {
        async fn load<'l, DB: Database, PK: Send + Sync + Encode<'l, DB> +
        Type<DB>, PKC: IntoIterator<Item=PK>>
        (executor: &sqlx::Pool<DB>, keys: PKC) -> sqlx::Result<Self>
            where Self: Sized
        {
            let mut iter = keys.into_iter();
            sqlx::query_as!(Table, "SELECT `a`, `b`, `c` FROM `table` WHERE `a` = ? AND `b` = ?",
                iter.nth(0).expect("PrimaryKey not set"), iter.nth(0).expect("PrimaryKey not set"))
                .fetch_one(executor).await
        }
    }

    let table = <Table as SmartQlObject>::load(&executor, [0u32, 1u32]).await.expect("Error in query");

    println!("a: {}, b: {}, c: {}", table.get_a(), table.get_b(), table.get_c());

    let mut table = smartql_init! {
        Table {
            a: 32,
            b: 0,
            c: "lol".to_string()
        }
    };
    table.decrement_a(32);
    println!("A: {}", table.get_a());
    println!("Changed fields: {:?}", table.get_delta());

    let mut lazy_table = smartql_init_lazy! {
        Table {
            a: 32,
            b: 0,
            c: "lol".to_string()
        }
    };
    lazy_table.decrement_a(32);
    println!("Lazy A: {}", lazy_table.get_a());
    println!("Lazy changed fields: {:?}", lazy_table.get_delta());
}