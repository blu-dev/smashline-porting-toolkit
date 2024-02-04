mod attrs;

use std::{
    collections::HashMap,
    path::PathBuf,
    process::{Command, Stdio},
};

use attrs::StatusAttrs;
use clap::Parser;
use hash40::Hash40;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

use crate::attrs::AcmdAttrs;

#[derive(Copy, Clone)]
enum StateCallback {
    Init,
    Start,
    End,
    Fini,
}

impl ToTokens for StateCallback {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Init => quote!(on_init).to_tokens(tokens),
            Self::Start => quote!(on_start).to_tokens(tokens),
            Self::End => quote!(on_end).to_tokens(tokens),
            Self::Fini => quote!(on_fini).to_tokens(tokens),
        }
    }
}

#[derive(Copy, Clone)]
enum StatusLine {
    Pre,
    Main,
    End,
    Init,
    Exec,
    ExecStop,
    Post,
    Exit,
    MapCorrection,
    FixCamera,
    FixPosSlow,
    CheckDamage,
    CheckAttack,
    OnChangeLr,
    LeaveStop,
    NotifyEventGimmick,
    CalcParam,
}

impl StatusLine {
    fn new(name: &str) -> Self {
        if name.contains("STATUS_PRE") {
            Self::Pre
        } else if name.contains("STATUS_MAIN") {
            Self::Main
        } else if name.contains("STATUS_END") {
            Self::End
        } else if name.contains("INIT_STATUS") {
            Self::Init
        } else if name.contains("EXEC_STATUS") {
            Self::Exec
        } else if name.contains("EXEC_STOP") {
            Self::ExecStop
        } else if name.contains("EXEC_STATUS_POST") {
            Self::Post
        } else if name.contains("EXIT_STATUS") {
            Self::Exit
        } else if name.contains("MAP_CORRECTION") {
            Self::MapCorrection
        } else if name.contains("FIX_CAMERA") {
            Self::FixCamera
        } else if name.contains("FIX_POS_SLOW") {
            Self::FixPosSlow
        } else if name.contains("CHECK_DAMAGE") {
            Self::CheckDamage
        } else if name.contains("CHECK_ATTACK") {
            Self::CheckAttack
        } else if name.contains("ON_CHANGE_LR") {
            Self::OnChangeLr
        } else if name.contains("LEAVE_STOP") {
            Self::LeaveStop
        } else if name.contains("NOTIFY_EVENT_GIMMICK") {
            Self::NotifyEventGimmick
        } else if name.contains("CALC_PARAM") {
            Self::CalcParam
        } else {
            panic!("Invalid status line {name}");
        }
    }
}

impl ToTokens for StatusLine {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Pre => quote::quote!(Pre).to_tokens(tokens),
            Self::Main => quote::quote!(Main).to_tokens(tokens),
            Self::End => quote::quote!(End).to_tokens(tokens),
            Self::Init => quote::quote!(Init).to_tokens(tokens),
            Self::Exec => quote::quote!(Exec).to_tokens(tokens),
            Self::ExecStop => quote::quote!(ExecStop).to_tokens(tokens),
            Self::Post => quote::quote!(Post).to_tokens(tokens),
            Self::Exit => quote::quote!(Exit).to_tokens(tokens),
            Self::MapCorrection => quote::quote!(MapCorrection).to_tokens(tokens),
            Self::FixCamera => quote::quote!(FixCamera).to_tokens(tokens),
            Self::FixPosSlow => quote::quote!(FixPosSlow).to_tokens(tokens),
            Self::CheckDamage => quote::quote!(CheckDamage).to_tokens(tokens),
            Self::CheckAttack => quote::quote!(CheckAttack).to_tokens(tokens),
            Self::OnChangeLr => quote::quote!(OnChangeLr).to_tokens(tokens),
            Self::LeaveStop => quote::quote!(LeaveStop).to_tokens(tokens),
            Self::NotifyEventGimmick => quote::quote!(NotifyEventGimmick).to_tokens(tokens),
            Self::CalcParam => quote::quote!(CalcParam).to_tokens(tokens),
        }
    }
}

#[derive(Copy, Clone)]
enum AcmdKind {
    Game,
    Effect,
    Sound,
    Expression,
}

impl ToTokens for AcmdKind {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Game => quote!(acmd).to_tokens(tokens),
            Self::Effect => quote!(acmd).to_tokens(tokens),
            Self::Sound => quote!(acmd).to_tokens(tokens),
            Self::Expression => quote!(acmd).to_tokens(tokens),
        }
    }
}

enum AgentBuilderCommand {
    Acmd {
        kind: AcmdKind,
        name: syn::Expr,
        function: syn::Ident,
    },
    Status {
        kind: syn::Expr,
        line: StatusLine,
        function: syn::Ident,
    },
    Opff {
        function: syn::Ident,
    },
    State {
        state: StateCallback,
        function: syn::Ident,
    },
}

impl ToTokens for AgentBuilderCommand {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Acmd {
                kind,
                name,
                function,
            } => quote!(#kind(#name, #function)).to_tokens(tokens),
            Self::Status {
                kind,
                line,
                function,
            } => quote!(status(#line, *#kind, #function)).to_tokens(tokens),
            Self::Opff { function } => quote!(on_line(Main, #function)).to_tokens(tokens),
            Self::State { state, function } => quote!(#state(#function)).to_tokens(tokens),
        }
    }
}

struct AgentBuilder {
    pub name: syn::Expr,
    pub commands: Vec<AgentBuilderCommand>,
}

impl ToTokens for AgentBuilder {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let Self { name, commands } = self;

        quote::quote! {
            smashline::Agent::new(#name)
                #(
                    .#commands
                )*
                .install();
        }
        .to_tokens(tokens);
    }
}

enum SpanCommand {
    Remove(Span),
    InsertExternC(Span),
}

#[derive(clap::Parser)]
pub struct PortingArgs {
    pub root_dir: PathBuf,
    pub default_agent: String,
}

fn main() {
    let args = PortingArgs::parse();

    for entry in walkdir::WalkDir::new(&args.root_dir) {
        let Ok(entry) = entry else {
            continue;
        };

        if !entry.file_name().to_str().unwrap().ends_with(".rs") {
            continue;
        }

        let input_file = std::fs::read_to_string(entry.path()).unwrap();

        let file: syn::File = syn::parse_str(&input_file).unwrap();

        let mut span_commands = vec![];
        let mut agents = HashMap::new();

        for item in file.items.iter() {
            match item {
                syn::Item::Fn(function) => {
                    if function.sig.ident == "install" {
                        span_commands.push(SpanCommand::Remove(function.span()));
                        continue;
                    }
                    for attr in function.attrs.iter() {
                        let last = attr.path().segments.last().unwrap();
                        if last.ident == "acmd_script" {
                            let attrs = attr.parse_args::<AcmdAttrs>().unwrap();
                            let builder =
                                agents.entry(attrs.agent.as_hash40()).or_insert_with(|| {
                                    AgentBuilder {
                                        name: attrs.agent.as_expr(),
                                        commands: vec![],
                                    }
                                });

                            let cat_ident =
                                attrs.category.segments.last().unwrap().ident.to_string();
                            let kind = match cat_ident.as_str() {
                                "ACMD_GAME" => AcmdKind::Game,
                                "ACMD_EFFECT" => AcmdKind::Effect,
                                "ACMD_SOUND" => AcmdKind::Sound,
                                "ACMD_EXPRESSION" => AcmdKind::Expression,
                                _ => unreachable!(),
                            };

                            if function.sig.abi.is_none() {
                                span_commands
                                    .push(SpanCommand::InsertExternC(function.sig.fn_token.span()));
                            }
                            span_commands.push(SpanCommand::Remove(attr.span()));

                            for script in attrs.scripts {
                                builder.commands.push(AgentBuilderCommand::Acmd {
                                    kind,
                                    name: script.as_expr(),
                                    function: function.sig.ident.clone(),
                                });
                            }
                        } else if last.ident == "status_script" {
                            let attrs = attr.parse_args::<StatusAttrs>().unwrap();
                            let builder =
                                agents.entry(attrs.agent.as_hash40()).or_insert_with(|| {
                                    AgentBuilder {
                                        name: attrs.agent.as_expr(),
                                        commands: vec![],
                                    }
                                });
                            if function.sig.abi.is_none() {
                                span_commands
                                    .push(SpanCommand::InsertExternC(function.sig.fn_token.span()));
                            }
                            span_commands.push(SpanCommand::Remove(attr.span()));

                            let line =
                                StatusLine::new(attrs.condition.as_ident().to_string().as_str());
                            builder.commands.push(AgentBuilderCommand::Status {
                                kind: attrs.status.as_expr(),
                                line,
                                function: function.sig.ident.clone(),
                            });
                        } else if last.ident == "opff" {
                            let attrs = attr.parse_args::<syn::Expr>().unwrap();
                            let syn::Expr::Path(syn::ExprPath { path, .. }) = attrs else {
                                panic!("Incorrect OPFF format");
                            };

                            let ident = path.segments.last().unwrap().ident.to_string();
                            let name = ident.strip_prefix("FIGHTER_KIND_").unwrap().to_lowercase();
                            let builder =
                                agents.entry(Hash40::new(name.as_str())).or_insert_with(|| {
                                    AgentBuilder {
                                        name: syn::Expr::Lit(syn::ExprLit {
                                            lit: syn::Lit::Str(syn::LitStr::new(
                                                &name,
                                                path.span(),
                                            )),
                                            attrs: vec![],
                                        }),
                                        commands: vec![],
                                    }
                                });

                            if function.sig.abi.is_none() {
                                span_commands
                                    .push(SpanCommand::InsertExternC(function.sig.fn_token.span()));
                            }
                            span_commands.push(SpanCommand::Remove(attr.span()));

                            builder.commands.push(AgentBuilderCommand::Opff {
                                function: function.sig.ident.clone(),
                            });
                        } else if last.ident == "fighter_init" || last.ident == "agent_init" {
                            let builder = agents
                                .entry(Hash40::new(&args.default_agent))
                                .or_insert_with(|| AgentBuilder {
                                    name: syn::Expr::Lit(syn::ExprLit {
                                        lit: syn::Lit::Str(syn::LitStr::new(
                                            &args.default_agent,
                                            last.ident.span(),
                                        )),
                                        attrs: vec![],
                                    }),
                                    commands: vec![],
                                });

                            if function.sig.abi.is_none() {
                                span_commands
                                    .push(SpanCommand::InsertExternC(function.sig.fn_token.span()));
                            }
                            span_commands.push(SpanCommand::Remove(attr.span()));

                            builder.commands.push(AgentBuilderCommand::State {
                                state: StateCallback::Init,
                                function: function.sig.ident.clone(),
                            });
                        } else if last.ident == "fighter_reset" || last.ident == "agent_reset" {
                            let builder = agents
                                .entry(Hash40::new(&args.default_agent))
                                .or_insert_with(|| AgentBuilder {
                                    name: syn::Expr::Lit(syn::ExprLit {
                                        lit: syn::Lit::Str(syn::LitStr::new(
                                            &args.default_agent,
                                            last.ident.span(),
                                        )),
                                        attrs: vec![],
                                    }),
                                    commands: vec![],
                                });

                            if function.sig.abi.is_none() {
                                span_commands
                                    .push(SpanCommand::InsertExternC(function.sig.fn_token.span()));
                            }
                            span_commands.push(SpanCommand::Remove(attr.span()));

                            builder.commands.push(AgentBuilderCommand::State {
                                state: StateCallback::Start,
                                function: function.sig.ident.clone(),
                            });
                        }
                    }
                }
                _ => {}
            }
        }

        span_commands.sort_by_key(|key| match key {
            SpanCommand::Remove(span) | SpanCommand::InsertExternC(span) => span.byte_range().start,
        });

        let mut file_data = input_file;
        for command in span_commands.into_iter().rev() {
            match command {
                SpanCommand::Remove(span) => {
                    file_data.replace_range(span.byte_range(), "");
                }
                SpanCommand::InsertExternC(span) => {
                    file_data.insert_str(span.byte_range().start, "extern \"C\" ");
                }
            }
        }

        let agents = agents.into_values();
        let mut tokens = quote::quote! {
            pub fn install() {
                #(#agents)*
            }
        }
        .to_string();

        // Format those tokens
        std::fs::write("tmp.rs", &tokens).unwrap();

        Command::new("rustfmt")
            .arg("--edition")
            .arg("2021")
            .arg("tmp.rs")
            .stderr(Stdio::null())
            .output()
            .unwrap();

        tokens = std::fs::read_to_string("tmp.rs").unwrap();

        file_data.push_str(tokens.as_str());

        std::fs::write(entry.path(), &file_data).unwrap();
        std::fs::remove_file("tmp.rs").unwrap();
    }
}
