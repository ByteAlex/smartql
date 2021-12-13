use smartql::{smartql_object, SmartQlObject, internal::{SmartQlMetaData}};

#[tokio::main]
pub async fn main() {
    let executor = sqlx::MySqlPool::connect(env!("DATABASE_URL"))
        .await.expect("Failed to connect to mysql");

    #[repr(i16)]
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub enum ReprEnum {
        Stuff = 0,
        Other = 1,
    }

    #[smartql_object(default, table = "table")]
    struct QueryTest {
        #[smartql(primary, incremental)]
        pub a: i32,
        #[smartql(primary, incremental, alias = "b")]
        pub secondary: i32,
        pub c: String,
        #[smartql(incremental, alias = "counter")]
        pub incremental_test: i32,
        #[smartql(repr = i16)]
        pub repr: Option<ReprEnum>,
    }

    let mut table = QueryTest::load(&executor, smartql::args!([0, 1]))
        .await
        .expect("Error in query")
        .expect("No result");

    println!("a: {}, b: {}, c: {}: counter: {}, repr: {:?}", table.get_a(), table.get_secondary(), table.get_c(), table.get_incremental_test(), table.get_repr());

    table.set_c("Fancy".to_owned());
    table.increment_incremental_test(1);
    table.set_repr(Some(ReprEnum::Other));

    println!("{:?}", table.get_delta());

    if !table.upsert(&executor).await.expect("Query failed") {
        panic!("Save failed")
    }

    println!("a: {}, b: {}, c: {}: counter: {}, repr: {:?}", table.get_a(), table.get_secondary(), table.get_c(), table.get_incremental_test(), table.get_repr());
    println!("{:?}", table.get_delta());
}