use smartql::{smartql_object, SmartQlObject, internal::{SmartQlMetaData}};
use async_trait::async_trait;

#[tokio::main]
pub async fn main() {
    let executor = sqlx::MySqlPool::connect(env!("DATABASE_URL"))
        .await.expect("Failed to connect to mysql");

    #[smartql_object]
    struct Table {
        #[smartql(primary, incremental)]
        pub a: i32,
        #[smartql(primary, incremental)]
        pub b: i32,
        pub c: String,
    }

    let mut table = Table::load(&executor, smartql::args!([0, 1]))
        .await
        .expect("Error in query")
        .expect("No result");

    println!("a: {}, b: {}, c: {}", table.get_a(), table.get_b(), table.get_c());

    //table.decrement_b(1);
    table.set_c("Fancy".to_owned());

    if !table.upsert(&executor).await.expect("Query failed") {
        panic!("Save failed")
    }

    println!("a: {}, b: {}, c: {}", table.get_a(), table.get_b(), table.get_c());
    println!("{:?}", table.get_delta());
}