extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use quote::ToTokens;
use std::str::FromStr;
use syn::{Attribute, NestedMeta, Meta, Lit};

fn to_string<T: ToTokens>(token: &T) -> String {
    let mut tokens = proc_macro2::TokenStream::new();
    token.to_tokens(&mut tokens);
    tokens.to_string()
}

fn into_smartql_group_content(attr: &Attribute) -> Option<proc_macro2::TokenStream> {
    if attr.path.segments.first().unwrap().ident.eq("smartql") {
        if let Some(next) = attr.tokens.clone().into_iter().next() {
            if let TokenTree::Group(group) = next {
                return Some(group.stream());
            }
        }
    }
    None
}

fn is_smartql_incremental(attr: &Attribute) -> bool {
    if let Some(content) = into_smartql_group_content(attr) {
        content.into_iter().any(|token| {
            if let TokenTree::Ident(ident) = token {
                ident.eq("incremental")
            } else {
                false
            }
        })
    } else {
        false
    }
}

fn is_smartql_primary(attr: &Attribute) -> bool {
    if let Some(content) = into_smartql_group_content(attr) {
        content.into_iter().any(|token| {
            if let TokenTree::Ident(ident) = token {
                ident.eq("primary")
            } else {
                false
            }
        })
    } else {
        false
    }
}

fn is_smartql_ignore(attr: &Attribute) -> bool {
    if let Some(content) = into_smartql_group_content(attr) {
        content.into_iter().any(|token| {
            if let TokenTree::Ident(ident) = token {
                ident.eq("ignore")
            } else {
                false
            }
        })
    } else {
        false
    }
}

fn get_smartql_alias(attr: &Attribute) -> Option<String> {
    if let Some(content) = into_smartql_group_content(attr) {
        let mut iter = content.into_iter();
        while let Some(token) = iter.next() {
            if let TokenTree::Ident(ident) = token {
                if ident.eq("alias") {
                    if let TokenTree::Punct(punct) = iter.next().expect("Alias expects a punctuation after it") {
                        if punct.as_char() == '=' {
                            if let TokenTree::Literal(lit) = iter.next().expect("Alias requires a literal after the equals sign") {
                                let lit: syn::LitStr = syn::parse2(lit.into_token_stream()).expect("Literal needs to be of type string");
                                return Some(lit.value());
                            }
                        } else {
                            panic!("Alias expects an equals punctuation!");
                        }
                    }
                }
            }
        }
    }
    None
}

fn get_smartql_repr(attr: &Attribute) -> Option<String> {
    if let Some(content) = into_smartql_group_content(attr) {
        let mut iter = content.into_iter();
        while let Some(token) = iter.next() {
            if let TokenTree::Ident(ident) = token {
                if ident.eq("repr") {
                    if let TokenTree::Punct(punct) = iter.next().expect("Alias expects a punctuation after it") {
                        if punct.as_char() == '=' {
                            let literal = iter.next().expect("Alias requires a literal after the equals sign");
                            if let TokenTree::Literal(lit) = literal {
                                let lit: syn::LitStr = syn::parse2(lit.into_token_stream()).expect("Literal needs to be of type string");
                                return Some(lit.value());
                            } else if let TokenTree::Ident(ident) = literal {
                                return Some(ident.to_string());
                            } else {
                                panic!("Matched unknown token in repr: {:?}", literal);
                            }
                        } else {
                            panic!("Alias expects an equals punctuation!");
                        }
                    }
                }
            }
        }
    }
    None
}

fn gen_accessors(field: &syn::Field, struct_name: &syn::Ident) -> proc_macro2::TokenStream {
    let ident = field.ident.clone().expect("Fields need identifiers!");
    let field_type = &field.ty;

    let getter = format!("get_{}", ident);
    let setter = format!("set_{}", ident);

    let getter = proc_macro2::TokenStream::from_str(getter.as_str()).unwrap();
    let setter = proc_macro2::TokenStream::from_str(setter.as_str()).unwrap();

    let mut setter_appendix = quote! {};

    let repr_fn = format!("__repr_{}", ident);
    let repr_fn = proc_macro2::TokenStream::from_str(repr_fn.as_str()).unwrap();
    let repr_method = if let Some(repr) = field.attrs.iter().flat_map(|attr| get_smartql_repr(attr)).next() {
        if field.attrs.iter().any(|attr| is_smartql_incremental(attr)) {
            panic!("Failure: A repr() field cannot be incremental")
        }

        let token = field_type.to_token_stream().into_iter()
            .next()
            .expect("Token required");

        let repr_token = proc_macro2::TokenStream::from_str(repr.as_str()).unwrap();

        if let TokenTree::Ident(field_type_ident) = token {
            if field_type_ident.to_string().eq("Option") {
                quote! {
                    pub fn #repr_fn(&self) -> Option < #repr_token > {
                        if let Some(data) = &self.#ident {
                            Option::Some(data.clone() as #repr_token)
                        } else {
                            Option::None
                        }
                    }
                }
            } else {
                quote! {
                    pub fn #repr_fn(&self) -> #repr_token {
                        data.clone() as #repr_token
                    }
                }
            }
        } else {
            quote! {
            pub fn #repr_fn(&self) -> #repr_token {
                data.clone() as #repr_token
            }
        }
        }
    } else {
        quote! {
            pub fn #repr_fn(&self) -> &#field_type {
                &self.#ident
            }
        }
    };

    let additional_methods = if field.attrs.iter().any(|attr| is_smartql_incremental(attr)) {
        let inc = format!("increment_{}", ident);
        let dec = format!("decrement_{}", ident);
        let delta_field = format!("self.__delta_{}", ident);

        let inc = proc_macro2::TokenStream::from_str(inc.as_str()).unwrap();
        let dec = proc_macro2::TokenStream::from_str(dec.as_str()).unwrap();
        let delta_field = proc_macro2::TokenStream::from_str(delta_field.as_str()).unwrap();

        setter_appendix = quote! {
            #delta_field = None;
        };

        quote! {
            pub fn #inc(&mut self, value: #field_type) {
                self.#ident = self.#ident + value;
                if smartql::internal::coerce_delta_op(&mut self.__field_delta,
                    stringify!(#struct_name), stringify!(#ident),
                    smartql::internal::DeltaOp::Incremental) == smartql::internal::DeltaOp::Set {
                    #delta_field = None;
                } else {
                    #delta_field = Some(value);
                }
            }

            pub fn #dec(&mut self, value: #field_type) {
                self.#ident = self.#ident - value;
                if smartql::internal::coerce_delta_op(&mut self.__field_delta,
                    stringify!(#struct_name), stringify!(#ident),
                    smartql::internal::DeltaOp::Incremental) == smartql::internal::DeltaOp::Set {
                    #delta_field = None;
                } else {
                    #delta_field = Some(value);
                }
            }
        }
    } else {
        quote! {}
    };

    quote! {
        pub fn #getter(&self) -> &#field_type {
            &self.#ident
        }

        pub fn #setter(&mut self, value: #field_type) {
            smartql::internal::coerce_delta_op(&mut self.__field_delta, stringify!(#struct_name), stringify!(#ident), smartql::internal::DeltaOp::Set);
            self.#ident = value;
            #setter_appendix
        }

        #additional_methods

        #repr_method
    }
}

#[proc_macro_derive(SmartQlObject, attributes(smartql))]
pub fn derive_smartql_object(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::ItemStruct);

    let ident = &input.ident;

    let primary_fields = input
        .fields
        .iter()
        .filter(|field| field.attrs.iter().any(|attr| is_smartql_primary(attr)))
        .map(|field| field.clone())
        .collect::<Vec<syn::Field>>();

    if primary_fields.len() == 0 {
        panic!(
            "Need at least one field marked with smartql(primary) for struct `{}`",
            ident
        );
    }

    let mut where_clause = "WHERE ".to_owned();
    let mut first = true;
    for field in primary_fields {
        let identifier = field.ident.expect("Fields need identifiers").to_string();
        let field_alias = field.attrs.iter()
            .flat_map(|attr| get_smartql_alias(attr))
            .next()
            .unwrap_or(identifier);

        if first {
            where_clause.push_str("`");
            where_clause.push_str(field_alias.as_str());
            where_clause.push_str("` = ?");
            first = false;
        } else {
            where_clause.push_str(" AND `");
            where_clause.push_str(field_alias.as_str());
            where_clause.push_str("` = ?");
        }
    }

    let mut upsert_bindings = "".to_owned();
    let mut upsert_bindings_appends = "".to_owned();
    let mut sql_fields_list = "".to_owned();
    let mut upsert_update_clause = "".to_owned();
    let mut first = true;
    let mut instance_creator_from_row = "".to_owned();
    let mut field_count = 0;
    for field in input.fields.iter() {
        if field.attrs.iter().any(|attr| is_smartql_ignore(attr)) {
            continue;
        }
        field_count = field_count + 1;
        let identifier = field.ident.clone().expect("Fields need identifiers").to_string();
        let field_alias = field.attrs.iter()
            .flat_map(|attr| get_smartql_alias(attr))
            .next()
            .unwrap_or(identifier.clone());

        if !field.attrs.iter().any(|attr| is_smartql_primary(attr)) {
            upsert_update_clause.push_str(format!("`{}` = ?, ", field_alias).as_str());
            upsert_bindings_appends.push_str(format!("self.__repr_{}(), ", identifier).as_str());
        }
        upsert_bindings.push_str(format!("self.__repr_{}(), ", identifier).as_str());

        if first {
            sql_fields_list.push_str("`");
            sql_fields_list.push_str(field_alias.as_str());
            sql_fields_list.push_str("`");
            first = false;
        } else {
            sql_fields_list.push_str(", `");
            sql_fields_list.push_str(field_alias.as_str());
            sql_fields_list.push_str("`");
        }

        if let Some(repr) = field.attrs.iter().flat_map(|attr| get_smartql_repr(attr)).next() {
            instance_creator_from_row.push_str(
                format!(
                    r#"unsafe {{ std::mem::transmute::<_, _>(row.get::<{}, _>("{}")) }}, "#,
                    repr, field_alias.as_str()
                )
                    .as_str(),
            );
        } else {
            instance_creator_from_row.push_str(
                format!(
                    r#"row.get("{}"), "#,
                    field_alias.as_str()
                )
                    .as_str(),
            );
        }
    }

    let upsert_update_clause = upsert_update_clause[..upsert_update_clause.len() - 2].to_owned();
    let mut upsert_value_placeholders = "?".to_owned();
    for _ in 1..field_count {
        upsert_value_placeholders.push_str(", ?")
    }

    upsert_bindings.push_str(upsert_bindings_appends.as_str());
    let upsert_bindings = proc_macro2::TokenStream::from_str(upsert_bindings.as_str()).unwrap();

    let instance_creator_from_row =
        proc_macro2::TokenStream::from_str(instance_creator_from_row.as_str()).unwrap();

    let select_clause = format!(
        "SELECT {} FROM `{{}}` {}",
        sql_fields_list.as_str(),
        where_clause.as_str()
    );

    let upsert_all_clause = format!(
        "INSERT INTO `{{}}` ({}) VALUES ({}) ON DUPLICATE KEY UPDATE {}",
        sql_fields_list,
        upsert_value_placeholders,
        upsert_update_clause
    );

    let upsert_prefix = format!(
        "INSERT INTO `{{}}` ({}) VALUES ({}) ON DUPLICATE KEY UPDATE ",
        sql_fields_list,
        upsert_value_placeholders
    );

    let result = quote! {
        #[smartql::async_trait]
        impl SmartQlObject for #ident {

            async fn load(executor: &sqlx::Pool<sqlx::MySql>, args: sqlx::mysql::MySqlArguments) -> sqlx::Result<Option<Self>>
                where Self: Sized {
                use smartql::internal::SmartQlMetaData;
                let row = sqlx::query_with(format!(#select_clause, #ident::table_name()).as_str(), args)
                    .fetch_optional(executor)
                    .await?;
                if let Some(row) = row {
                    use sqlx::Row;
                    return Ok(Some(Self::new(#instance_creator_from_row)));
                } else {
                    return Ok(None);
                }
            }

            async fn save_all(&mut self, executor: &sqlx::Pool<sqlx::MySql>) -> sqlx::Result<bool> {
                use smartql::internal::SmartQlMetaData;
                let result = sqlx::query_with(format!(#upsert_all_clause, #ident::table_name()).as_str(), smartql::args!([#upsert_bindings]))
                    .execute(executor)
                    .await?;
                self.reset_delta();
                Ok(result.rows_affected() > 0)
            }

            async fn upsert(&mut self, executor: &sqlx::Pool<sqlx::MySql>) -> sqlx::Result<bool> {
                use smartql::internal::SmartQlMetaData;
                let mut upsert = format!(#upsert_prefix, #ident::table_name());
                let delta = self.get_delta();
                let mut args = sqlx::mysql::MySqlArguments::default();
                for field in Self::fields().into_iter() {
                    self.add_field_to_args(&mut args, field);
                }
                for (field, delta_op) in delta.clone().into_iter() {
                    match delta_op {
                        smartql::internal::DeltaOp::Set => {
                            self.add_field_to_args(&mut args, field);
                            upsert.push_str(format!("`{}` = ?, ", #ident::field_alias(field)).as_str());
                        }
                        smartql::internal::DeltaOp::Incremental => {
                            self.add_delta_field_to_args(&mut args, field);
                            upsert.push_str(format!("`{}` = `{}` + ?, ", #ident::field_alias(field), #ident::field_alias(field)).as_str());
                        }
                    }
                }
                let upsert = upsert[..upsert.len() - 2].to_owned();
                let result = sqlx::query_with(upsert.as_str(), args)
                    .execute(executor)
                    .await?;
                self.reset_delta();
                Ok(result.rows_affected() > 0)
            }
        }
    };

    return result.into();
}

#[proc_macro_attribute]
pub fn smartql_object(attr: TokenStream, item: TokenStream) -> TokenStream {
    let struct_item = syn::parse_macro_input!(item as syn::ItemStruct);
    let struct_ident = &struct_item.ident;

    let mut table = format!("{}", struct_ident).to_lowercase();

    let args = syn::parse_macro_input!(attr as syn::AttributeArgs);
    for meta in args {
        match meta {
            NestedMeta::Meta(meta) => {
                match meta {
                    Meta::Path(_) => {}
                    Meta::List(_) => {}
                    Meta::NameValue(pair) => {
                        let key = to_string(&pair.path);
                        if key.eq("table") {
                            if let Lit::Str(key) = pair.lit {
                                table = key.value();
                            } else {
                                panic!("Invalid literal for `table` specified")
                            }
                        }
                    }
                }
            }
            NestedMeta::Lit(literal) => {
                match literal {
                    Lit::Str(_) => {}
                    Lit::ByteStr(_) => {}
                    Lit::Byte(_) => {}
                    Lit::Char(_) => {}
                    Lit::Int(_) => {}
                    Lit::Float(_) => {}
                    Lit::Bool(_) => {}
                    Lit::Verbatim(_) => {}
                }
            }
        }
    }

    let mut accessors = "".to_owned();

    let mut alias_match_pattern = "".to_owned();

    let mut match_pattern = "".to_owned();
    let mut delta_match_pattern = "".to_owned();

    let mut delta_fields_reset = "".to_owned();
    let mut delta_fields = "".to_owned();
    let mut delta_fields_initializer = "".to_owned();
    let mut fields_initializer = "".to_owned();
    let mut fields_with_type = "".to_owned();
    let mut fields_list = "".to_owned();
    let mut fields = "".to_owned();
    for field in struct_item.fields {
        for attr in &field.attrs {
            fields.push_str(to_string(&attr).as_str());
            fields.push_str("\n");
        }

        let field_ident = field
            .ident
            .clone()
            .expect("Fields need identifiers!")
            .to_string();

        if !field.attrs.iter().any(|attr| is_smartql_ignore(attr)) {
            if let Some(alias) = field.attrs.iter().flat_map(|attr| get_smartql_alias(attr))
                .next() {
                alias_match_pattern.push_str(format!(
                    r#""{}" => "{}", "#,
                    field_ident, alias,
                ).as_str());
            }

            match_pattern.push_str(
                format!(
                    r#""{}" => args.add(self.__repr_{}()), "#,
                    field_ident, field_ident
                )
                    .as_str(),
            );
            fields_with_type.push_str(format!(
                "{}: {}, ",
                field_ident, to_string(&field.ty)
            ).as_str());
            fields_initializer.push_str(format!("{}: {}, ", field_ident, field_ident).as_str());

            if field.attrs.iter().any(|attr| is_smartql_incremental(attr)) {
                delta_match_pattern.push_str(
                    format!(
                        r#""{}" => args.add(self.__delta_{}), "#,
                        field_ident, field_ident
                    )
                        .as_str(),
                );
                delta_fields.push_str(format!(
                    "#[smartql(ignore)] __delta_{}: Option<{}>, ",
                    field_ident, to_string(&field.ty)
                ).as_str());
                delta_fields_initializer.push_str(format!(
                    "__delta_{}: None,",
                    field_ident
                ).as_str());
                delta_fields_reset.push_str(format!(
                    "self.__delta_{} = None; ",
                    field_ident
                ).as_str());
            }
        }

        fields_list.push_str("\"");
        fields_list.push_str(field_ident.as_str());
        fields_list.push_str("\"");
        fields_list.push_str(",");

        fields.push_str(field_ident.as_str());
        fields.push_str(": ");
        fields.push_str(to_string(&field.ty).as_str());
        fields.push_str(",\n\n");
        accessors.push_str(&gen_accessors(&field, &struct_ident).to_string());
        accessors.push_str("\n");
    }

    let alias_match_pattern = proc_macro2::TokenStream::from_str(alias_match_pattern.as_str()).unwrap();
    let delta_fields_reset = proc_macro2::TokenStream::from_str(delta_fields_reset.as_str()).unwrap();
    let delta_fields_initializer = proc_macro2::TokenStream::from_str(delta_fields_initializer.as_str()).unwrap();
    let delta_fields = proc_macro2::TokenStream::from_str(delta_fields.as_str()).unwrap();
    let fields_initializer = proc_macro2::TokenStream::from_str(fields_initializer.as_str()).unwrap();
    let fields_with_type = proc_macro2::TokenStream::from_str(fields_with_type.as_str()).unwrap();
    let match_pattern = proc_macro2::TokenStream::from_str(match_pattern.as_str()).unwrap();
    let delta_match_pattern = proc_macro2::TokenStream::from_str(delta_match_pattern.as_str()).unwrap();
    let fields_token = proc_macro2::TokenStream::from_str(fields.as_str()).unwrap();
    let fields_list_token = proc_macro2::TokenStream::from_str(fields_list.as_str()).unwrap();
    let accessors_token = proc_macro2::TokenStream::from_str(accessors.as_str()).unwrap();

    let result = quote! {
        #[derive(smartql::SmartQlObject)]
        pub struct #struct_ident {
            #[smartql(ignore)]
            __field_delta: std::collections::HashMap<&'static str, smartql::internal::DeltaOp>,
            #fields_token
            #delta_fields
        }

        impl #struct_ident {

            pub fn new( #fields_with_type ) -> Self {
                Self {
                    __field_delta: std::collections::HashMap::new(),
                    #fields_initializer
                    #delta_fields_initializer
                }
            }

            pub fn new_with_delta(delta: std::collections::HashMap<&'static str, smartql::internal::DeltaOp>, #fields_with_type ) -> Self {
                Self {
                    __field_delta: delta,
                    #fields_initializer
                    #delta_fields_initializer
                }
            }

            #accessors_token
        }

        impl smartql::internal::SmartQlMetaData for #struct_ident {
            fn table_name() -> &'static str {
                return #table;
            }

            fn fields() -> Vec<&'static str> {
                return vec![#fields_list_token]
            }

            fn field_alias(field: &'static str) -> &'static str {
                match field {
                    #alias_match_pattern
                    _ => field,
                }
            }

            fn get_delta(&self) -> &std::collections::HashMap<&'static str, smartql::internal::DeltaOp> {
                &self.__field_delta
            }

            fn reset_delta(&mut self) {
                self.__field_delta = std::collections::HashMap::new();
                #delta_fields_reset
            }

            fn add_field_to_args(&self, args: &mut sqlx::mysql::MySqlArguments, field: &'static str) {
                use sqlx::Arguments;
                match field {
                    #match_pattern
                    _ => {
                        panic!("Unknown field!");
                    }
                }
            }

            fn add_delta_field_to_args(&self, args: &mut sqlx::mysql::MySqlArguments, field: &'static str) {
                use sqlx::Arguments;
                match field {
                    #delta_match_pattern
                    _ => {
                        panic!("Unknown field!");
                    }
                }
            }
        }
    };

    return result.into();
}

#[proc_macro]
pub fn args(item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(item as syn::ExprArray);

    let mut binds = "".to_owned();

    for arg in args.elems {
        binds.push_str(format!("args.add({}); ", arg.to_token_stream()).as_str())
    }

    let binds = proc_macro2::TokenStream::from_str(binds.as_str()).unwrap();

    let result = quote! {
        {
            use sqlx::{Arguments};
            let mut args = sqlx::mysql::MySqlArguments::default();
            #binds
            args
        }
    };

    return result.into();
}

#[proc_macro]
pub fn smartql_init(item: TokenStream) -> TokenStream {
    let struct_item = syn::parse_macro_input!(item as syn::ExprStruct);

    let struct_path = &struct_item.path;

    let mut field_values = "".to_owned();
    let mut field_names = "".to_owned();
    for field in &struct_item.fields {
        if let syn::Member::Named(ident) = &field.member {
            let field_ident = to_string(ident);
            field_names.push_str(format!(
                r#"("{}", smartql::internal::DeltaOp::Set),"#,
                field_ident
            ).as_str());
            field_values.push_str(format!(
                r#"{},"#,
                to_string(&field.expr)
            ).as_str());
        }
    }

    let delta_assignment = format!("[{}].iter().cloned().collect()", field_names);
    let delta_assignment = proc_macro2::TokenStream::from_str(delta_assignment.as_str())
        .unwrap();

    let field_assignment = proc_macro2::TokenStream::from_str(field_values.as_str())
        .unwrap();

    return quote! {
        #struct_path::new_with_delta(#delta_assignment, #field_assignment)
    }
        .into();
}

#[proc_macro]
pub fn smartql_init_lazy(item: TokenStream) -> TokenStream {
    let struct_item = syn::parse_macro_input!(item as syn::ExprStruct);

    let struct_path = &struct_item.path;

    let mut field_values = "".to_owned();

    for field in &struct_item.fields {
        if let syn::Member::Named(_ident) = &field.member {
            field_values.push_str(format!(
                r#"{},"#,
                to_string(&field.expr)
            ).as_str());
        }
    }

    let field_assignment = proc_macro2::TokenStream::from_str(field_values.as_str())
        .unwrap();

    return quote! {
        #struct_path::new(#field_assignment)
    }.into();
}
