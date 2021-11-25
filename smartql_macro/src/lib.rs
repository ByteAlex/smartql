extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use std::str::FromStr;
use quote::ToTokens;
use syn::Attribute;
use proc_macro2::TokenTree;

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

fn gen_accessors(field: &syn::Field, struct_name: &syn::Ident) -> proc_macro2::TokenStream {
    let ident = field.ident.clone().expect("Fields need identifiers!");
    let field_type = &field.ty;

    let getter = format!("get_{}", ident);
    let setter = format!("set_{}", ident);

    let getter = proc_macro2::TokenStream::from_str(getter.as_str()).unwrap();
    let setter = proc_macro2::TokenStream::from_str(setter.as_str()).unwrap();

    let additional_methods = if field.attrs.iter().any(|attr| is_smartql_incremental(attr)) {
        let inc = format!("increment_{}", ident);
        let dec = format!("decrement_{}", ident);

        let inc = proc_macro2::TokenStream::from_str(inc.as_str()).unwrap();
        let dec = proc_macro2::TokenStream::from_str(dec.as_str()).unwrap();

        quote! {
            pub fn #inc(&mut self, value: #field_type) {
                self.#ident = self.#ident + value;
                smartql::internal::coerce_delta_op(&mut self.__field_delta, stringify!(#struct_name), stringify!(#ident), smartql::internal::DeltaOp::Increment);
            }

            pub fn #dec(&mut self, value: #field_type) {
                self.#ident = self.#ident - value;
                smartql::internal::coerce_delta_op(&mut self.__field_delta, stringify!(#struct_name), stringify!(#ident), smartql::internal::DeltaOp::Decrement);
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
        }

        #additional_methods
    }
}

#[proc_macro_derive(SmartQlObject, attributes(smartql))]
pub fn derive_smartql_object(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::ItemStruct);

    let ident = &input.ident;

    let primary_fields = input.fields.iter()
        .filter(|field| field.attrs.iter().any(|attr| is_smartql_primary(attr)))
        .map(|field| field.clone())
        .collect::<Vec<syn::Field>>();

    if primary_fields.len() == 0 {
        panic!("Need at least one field marked with smartql(primary) for struct `{}`", ident);
    }

    let mut primary_bind_expr_load = "".to_owned();
    let mut primary_bind_expr_save = "".to_owned();
    let mut where_clause = "WHERE ".to_owned();
    let mut first = true;
    for field in primary_fields {
        let identifier = field.ident.expect("Fields need identifiers").to_string();
        if first {
            where_clause.push_str("`");
            where_clause.push_str(identifier.as_str());
            where_clause.push_str("` = ?");
            first = false;
        } else {
            where_clause.push_str(" AND `");
            where_clause.push_str(identifier.as_str());
            where_clause.push_str("` = ?");
        }

        primary_bind_expr_load.push_str(", iter.nth(0).expect(\"PrimaryKey not set\")");

        primary_bind_expr_save.push_str(", self.get_");
        primary_bind_expr_save.push_str(identifier.as_str());
        primary_bind_expr_save.push_str("()");
    }
    let primary_bind_expr_load = proc_macro2::TokenStream::from_str(primary_bind_expr_load.as_str()).unwrap();
    let primary_bind_expr_save = proc_macro2::TokenStream::from_str(primary_bind_expr_save.as_str()).unwrap();

    let mut sql_fields_list = "".to_owned();
    let mut first = true;
    for field in input.fields.iter() {
        if field.attrs.iter().any(|attr| is_smartql_ignore(attr)) {
            continue;
        }
        let identifier = field.ident.clone().expect("Fields need identifiers").to_string();
        if first {
            sql_fields_list.push_str("`");
            sql_fields_list.push_str(identifier.as_str());
            sql_fields_list.push_str("`");
            first = false;
        } else {
            sql_fields_list.push_str(", `");
            sql_fields_list.push_str(identifier.as_str());
            sql_fields_list.push_str("`");
        }
    }

    let table = format!("{}", ident).to_lowercase();

    let mut select_clause = "SELECT ".to_owned();
    select_clause.push_str(sql_fields_list.as_str());
    select_clause.push_str(" FROM `");
    select_clause.push_str(table.as_str());
    select_clause.push_str("` ");
    select_clause.push_str(where_clause.as_str());


    println!("Select clause: {}", select_clause);

    let result = quote! {
        use async_trait::async_trait;
        use sqlx::{Encode, Database, Type};

        #[async_trait]
        impl SmartQlObject for #ident {
            async fn load<'l, DB: Database, PK: Send + Sync + Encode<'l, DB> + Type<DB>,
                PKC: IntoIterator<Item=PK>>(executor: &sqlx::Pool<DB>, keys: PKC) -> sqlx::Result<Self>
                where Self: Sized {
                let mut iter = keys.into_iter();
                sqlx::query_as!(#ident, #select_clause #primary_bind_expr_load).fetch_one(executor).await
            }

/*          fn query<B: QueryBuilder<Self>>() -> B {

            }

            async fn save(&mut self) -> sqlx::Result<()> {

            }*/
        }
    };

    println!("{}", result);

    return quote! {}.into();
    //return result.into();
}

#[proc_macro_attribute]
pub fn smartql_object(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let struct_item = syn::parse_macro_input!(item as syn::ItemStruct);
    let struct_ident = &struct_item.ident;

    let mut accessors = "".to_owned();

    let mut fields_list = "".to_owned();
    let mut fields = "".to_owned();
    for field in struct_item.fields {
        for attr in &field.attrs {
            fields.push_str(to_string(&attr).as_str());
            fields.push_str("\n");
        }
        let field_ident = field.ident.clone().expect("Fields need identifiers!").to_string();

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

    let fields_token = proc_macro2::TokenStream::from_str(fields.as_str()).unwrap();
    let fields_list_token = proc_macro2::TokenStream::from_str(fields_list.as_str()).unwrap();
    let accessors_token = proc_macro2::TokenStream::from_str(accessors.as_str()).unwrap();

    let result = quote! {
        #[derive(smartql::SmartQlObject, sqlx::FromRow)]
        pub struct #struct_ident {
            #[smartql(ignore)]
            __field_delta: std::collections::HashMap<&'static str, smartql::internal::DeltaOp>,
            #fields_token
        }

        impl #struct_ident {
            #accessors_token
        }

        impl smartql::internal::SmartQlMetaData for #struct_ident {
            fn fields() -> Vec<&'static str> {
                return vec![#fields_list_token]
            }

            fn get_delta(&self) -> &std::collections::HashMap<&'static str, smartql::internal::DeltaOp> {
                &self.__field_delta
            }

            fn reset_delta(&mut self) {
                self.__field_delta = std::collections::HashMap::new();
            }
        }
    };

    return result.into();
}

#[proc_macro]
pub fn smartql_init(item: TokenStream) -> TokenStream {
    let mut struct_item = syn::parse_macro_input!(item as syn::ExprStruct);

    let mut field_names = "".to_owned();
    for field in &struct_item.fields {
        if let syn::Member::Named(ident) = &field.member {
            field_names.push_str("(\"");
            field_names.push_str(to_string(ident).as_str());
            field_names.push_str("\", smartql::internal::DeltaOp::Set)");
            field_names.push_str(",")
        }
    }

    let assignment = format!("__field_delta: [{}].iter().cloned().collect()", field_names);
    let token = proc_macro2::TokenStream::from_str(assignment.as_str()).unwrap().into();
    struct_item.fields.insert(0, syn::parse_macro_input!(token as syn::FieldValue));

    return quote! {
        #struct_item
    }.into();
}

#[proc_macro]
pub fn smartql_init_lazy(item: TokenStream) -> TokenStream {
    let mut struct_item = syn::parse_macro_input!(item as syn::ExprStruct);

    let assignment = "__field_delta: std::collections::HashMap::new()";
    let token = proc_macro2::TokenStream::from_str(assignment).unwrap().into();
    struct_item.fields.insert(0, syn::parse_macro_input!(token as syn::FieldValue));

    return quote! {
        #struct_item
    }.into();
}
