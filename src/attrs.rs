use hash40::Hash40;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{bracketed, punctuated, token, Token};

mod kw {
    syn::custom_keyword!(module);
    syn::custom_keyword!(symbol);
    syn::custom_keyword!(agent);
    syn::custom_keyword!(script);
    syn::custom_keyword!(scripts);
    syn::custom_keyword!(category);
    syn::custom_keyword!(low_priority);
    syn::custom_keyword!(status);
    syn::custom_keyword!(condition);
    syn::custom_keyword!(global);
}

// taken from skyline-rs hooking implementation
pub struct MetaItem<Keyword: Parse, Item: Parse> {
    pub ident: Keyword,
    pub item: Item,
}

impl<Keyword: Parse, Item: Parse> Parse for MetaItem<Keyword, Item> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let item = if input.peek(token::Paren) {
            let content;
            syn::parenthesized!(content in input);
            content.parse()?
        } else {
            input.parse::<Token![=]>()?;
            input.parse()?
        };

        Ok(Self { ident, item })
    }
}

struct BracketedList<Keyword: Parse, Item: Parse> {
    pub ident: Keyword,
    pub list: punctuated::Punctuated<Item, syn::Token![,]>,
}

impl<Keyword: Parse, Item: Parse> Parse for BracketedList<Keyword, Item> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let _: Token![=] = input.parse()?;
        let list = if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            Ok(content.parse_terminated(Item::parse, syn::Token![,])?)
        } else {
            Err(input.error("Could not find bracketed list."))
        }?;

        Ok(Self { ident, list })
    }
}

pub enum HookModule {
    Lazy(syn::LitStr),
    Static(token::Static),
}

impl Parse for HookModule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(module_name) = input.parse::<syn::LitStr>() {
            Ok(HookModule::Lazy(module_name))
        } else {
            let static_kw = input.parse()?;
            Ok(HookModule::Static(static_kw))
        }
    }
}

pub enum HookSymbol {
    Resolved(syn::Path),
    Unresolved(syn::LitStr),
}

impl Parse for HookSymbol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(symbol) = input.parse::<syn::LitStr>() {
            Ok(HookSymbol::Unresolved(symbol))
        } else {
            let symbol = input.parse()?;
            Ok(HookSymbol::Resolved(symbol))
        }
    }
}

pub struct HookAttrs {
    pub module: HookModule,
    pub symbol: HookSymbol,
}

impl Parse for HookAttrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let module = if input.peek(kw::module) {
            let MetaItem::<kw::module, HookModule> {
                item: module_name, ..
            } = input.parse()?;

            Ok(module_name)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "module"
            )))
        }?;

        let _: syn::Token![,] = input.parse()?;

        let symbol = if input.peek(kw::symbol) {
            let MetaItem::<kw::symbol, HookSymbol> { item: symbol, .. } = input.parse()?;
            Ok(symbol)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "symbol"
            )))
        }?;

        Ok(HookAttrs { module, symbol })
    }
}

#[derive(Debug, Clone)]
pub enum Hashable {
    Literal(syn::LitStr),
    Constant(syn::Path),
    Hashed(syn::Expr),
}

impl Hashable {
    pub fn as_expr(&self) -> syn::Expr {
        match self {
            Self::Literal(lit) => syn::Expr::Lit(syn::ExprLit {
                attrs: vec![],
                lit: syn::Lit::Str(lit.clone()),
            }),
            Self::Constant(path) => syn::Expr::Path(syn::ExprPath {
                attrs: vec![],
                qself: None,
                path: path.clone(),
            }),
            Self::Hashed(hashed) => hashed.clone(),
        }
    }

    pub fn as_hash40(&self) -> Hash40 {
        match self {
            Self::Literal(literal) => Hash40::new(literal.value().as_str()),
            Self::Hashed(expr) => match expr {
                syn::Expr::Lit(literal) => match &literal.lit {
                    syn::Lit::Int(integer) => Hash40(
                        u64::from_str_radix(integer.to_string().strip_prefix("0x").unwrap(), 16)
                            .unwrap(),
                    ),
                    _ => panic!("Invalid literal"),
                },
                _ => panic!("Invalid expression"),
            },
            _ => panic!("Constant not supported"),
        }
    }
}

impl ToTokens for Hashable {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Hashable::Literal(lit) => quote!(
                smash::phx::Hash40::new(#lit)
            )
            .to_tokens(tokens),
            Hashable::Constant(path) => quote!(
                smash::phx::Hash40::new_raw(#path)
            )
            .to_tokens(tokens),
            Hashable::Hashed(expr) => quote!(
                smash::phx::Hash40::new_raw(#expr)
            )
            .to_tokens(tokens),
        }
    }
}

impl Parse for Hashable {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(constant) = input.parse::<syn::Path>() {
            Ok(Hashable::Constant(constant))
        } else {
            let hashed = input.parse::<syn::Expr>()?;
            if let syn::Expr::Lit(syn::ExprLit {
                attrs: _,
                lit: syn::Lit::Str(lit),
            }) = hashed
            {
                Ok(Hashable::Literal(lit))
            } else {
                Ok(Hashable::Hashed(hashed))
            }
        }
    }
}

#[derive(Debug)]
pub struct AcmdAttrs {
    pub agent: Hashable,
    pub scripts: Vec<Hashable>,
    pub category: syn::Path,
    pub low_priority: syn::LitBool,
}

impl Parse for AcmdAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let agent = if input.peek(kw::agent) {
            let MetaItem::<kw::agent, Hashable> { item: hashable, .. } = input.parse()?;

            Ok(hashable)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "agent"
            )))
        }?;

        let _: Token![,] = input.parse()?;
        let scripts = if input.peek(kw::script) {
            let MetaItem::<kw::script, Hashable> { item: hashable, .. } = input.parse()?;

            Ok(vec![hashable])
        } else if input.peek(kw::scripts) {
            let BracketedList::<kw::scripts, Hashable> {
                list: hashables, ..
            } = input.parse()?;

            let scripts = hashables.into_iter().map(|x| x).collect();
            Ok(scripts)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' or '{}' in macro declaration.",
                "script", "scripts"
            )))
        }?;

        let _: Token![,] = input.parse()?;
        let category = if input.peek(kw::category) {
            let MetaItem::<kw::category, syn::Path> { item: category, .. } = input.parse()?;

            Ok(category)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "category"
            )))
        }?;

        let low_priority = if let Ok(_) = input.parse::<Token![,]>() {
            if let Ok(_) = input.parse::<kw::low_priority>() {
                Ok(syn::LitBool::new(true, Span::call_site()))
            } else {
                Err(input.error("Extra comma in macro declaration."))
            }
        } else {
            Ok(syn::LitBool::new(false, Span::call_site()))
        }?;

        Ok(Self {
            agent,
            scripts,
            category,
            low_priority,
        })
    }
}

#[derive(Debug)]
pub enum LuaConst {
    Symbolic(syn::Path),
    Evaluated(syn::Expr),
}

impl LuaConst {
    pub fn as_ident(&self) -> syn::Ident {
        match self {
            Self::Symbolic(path) => path.segments.last().unwrap().ident.clone(),
            _ => panic!("Hardcoded constants are not supported for conditions"),
        }
    }

    pub fn as_expr(&self) -> syn::Expr {
        match self {
            Self::Symbolic(path) => syn::Expr::Path(syn::ExprPath {
                attrs: vec![],
                qself: None,
                path: path.clone(),
            }),
            Self::Evaluated(expr) => expr.clone(),
        }
    }
}

impl ToTokens for LuaConst {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            LuaConst::Symbolic(path) => quote!(
                smashline::LuaConstant::Symbolic(#path)
            )
            .to_tokens(tokens),
            LuaConst::Evaluated(expr) => quote!(
                smashline::LuaConstant::Evaluated(#expr)
            )
            .to_tokens(tokens),
        }
    }
}

impl Parse for LuaConst {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(symbolic) = input.parse::<syn::Path>() {
            Ok(LuaConst::Symbolic(symbolic))
        } else {
            let evaluated = input.parse()?;
            Ok(LuaConst::Evaluated(evaluated))
        }
    }
}

#[derive(Debug)]
pub struct StatusAttrs {
    pub agent: Hashable,
    pub status: LuaConst,
    pub condition: LuaConst,
    pub low_priority: syn::LitBool,
}

impl Parse for StatusAttrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let agent = if input.peek(kw::agent) {
            let MetaItem::<kw::agent, Hashable> { item: hashable, .. } = input.parse()?;

            Ok(hashable)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "agent"
            )))
        }?;

        let _: Token![,] = input.parse()?;

        let status = if input.peek(kw::status) {
            let MetaItem::<kw::status, LuaConst> {
                item: lua_const, ..
            } = input.parse()?;

            Ok(lua_const)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "status"
            )))
        }?;

        let _: Token![,] = input.parse()?;

        let condition = if input.peek(kw::condition) {
            let MetaItem::<kw::condition, LuaConst> {
                item: lua_const, ..
            } = input.parse()?;

            Ok(lua_const)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "condition"
            )))
        }?;

        let low_priority = if let Ok(_) = input.parse::<Token![,]>() {
            if let Ok(_) = input.parse::<kw::low_priority>() {
                Ok(syn::LitBool::new(true, Span::call_site()))
            } else {
                Err(input.error("Extra comma in macro declaration."))
            }
        } else {
            Ok(syn::LitBool::new(false, Span::call_site()))
        }?;

        Ok(Self {
            agent,
            status,
            condition,
            low_priority,
        })
    }
}

pub struct CommonStatusAttrs {
    pub status: LuaConst,
    pub condition: LuaConst,
    pub symbol: Option<syn::LitStr>,
}

impl Parse for CommonStatusAttrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let status = if input.peek(kw::status) {
            let MetaItem::<kw::status, LuaConst> {
                item: lua_const, ..
            } = input.parse()?;

            Ok(lua_const)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "status"
            )))
        }?;

        let _: Token![,] = input.parse()?;

        let condition = if input.peek(kw::condition) {
            let MetaItem::<kw::condition, LuaConst> {
                item: lua_const, ..
            } = input.parse()?;

            Ok(lua_const)
        } else {
            Err(input.error(format!(
                "Expected keyword '{}' in macro declaration.",
                "condition"
            )))
        }?;

        let symbol = if let Ok(_) = input.parse::<Token![,]>() {
            if let Ok(MetaItem::<kw::symbol, syn::LitStr> { item: symbol, .. }) =
                input.parse::<MetaItem<kw::symbol, syn::LitStr>>()
            {
                Ok(Some(symbol))
            } else {
                Err(input.error("Extra comma in macro declaration."))
            }
        } else {
            Ok(None)
        }?;

        Ok(CommonStatusAttrs {
            status,
            condition,
            symbol,
        })
    }
}

pub struct AgentFrameAttrs {
    pub agent: Option<LuaConst>,
    pub is_replace: bool,
}

impl Parse for AgentFrameAttrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let agent = if input.peek(kw::agent) {
            let MetaItem::<kw::agent, LuaConst> {
                item: lua_const, ..
            } = input.parse()?;

            Ok(Some(lua_const))
        } else if input.peek(kw::global) {
            let _: kw::global = input.parse()?;
            Ok(None)
        } else {
            Err(input.error(format!(
                "Expected keywords '{}' or '{}' in macro declaration.",
                "agent", "global"
            )))
        }?;

        let is_replace = if let Ok(_) = input.parse::<Token![,]>() {
            if let Ok(_) = input.parse::<Token![override]>() {
                Ok(true)
            } else {
                Err(input.error("Extra comma in macro declaration."))
            }
        } else {
            Ok(false)
        }?;

        Ok(Self { agent, is_replace })
    }
}
