use sqlx::mysql::MySqlArguments;
use std::collections::HashMap;

pub trait SmartQlMetaData {
    fn table_name() -> &'static str;

    fn fields() -> Vec<&'static str>;

    fn get_delta(&self) -> &HashMap<&'static str, DeltaOp>;

    fn reset_delta(&mut self);

    fn add_field_to_args(&self, args: &mut MySqlArguments, field: &'static str);

    fn add_delta_field_to_args(&self, args: &mut sqlx::mysql::MySqlArguments, field: &'static str);
}

pub fn coerce_delta_op(
    map: &mut HashMap<&'static str, DeltaOp>,
    r#struct: &'static str,
    field: &'static str,
    new_op: DeltaOp,
) -> DeltaOp {
    if let Some(prev) = map.insert(field, new_op) {
        if new_op != DeltaOp::Set {
            if prev != DeltaOp::Set {
                log::warn!("Field {} on struct {} was updated (incrementally) twice without saving; Falling back to SET operation.", field, r#struct);
            }
            map.insert(field, DeltaOp::Set);
            return DeltaOp::Set;
        }
    }
    new_op
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DeltaOp {
    Incremental,
    Set,
}
