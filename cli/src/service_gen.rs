//! `ad4m service-gen`: generated surfaces from one service interface
//! document (SPEC_SERVICE_LANGUAGES §12).

use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Args;
use rust_executor::services::{codegen, InterfaceDocument};

#[derive(Debug, Args)]
pub struct ServiceGenArgs {
    /// The interface document (JSON).
    interface: PathBuf,
    /// Write the TypeScript client module here.
    #[arg(long)]
    ts: Option<PathBuf>,
    /// Write the MCP tool descriptors (JSON) here.
    #[arg(long)]
    mcp: Option<PathBuf>,
    /// Write the Markdown reference page here.
    #[arg(long)]
    docs: Option<PathBuf>,
}

pub fn run(args: ServiceGenArgs) -> Result<()> {
    let text = std::fs::read_to_string(&args.interface)
        .with_context(|| format!("cannot read {}", args.interface.display()))?;
    let raw: serde_json::Value = serde_json::from_str(&text)
        .with_context(|| format!("{} is not JSON", args.interface.display()))?;
    let doc = InterfaceDocument::parse(raw).map_err(anyhow::Error::msg)?;
    println!("{} {}", doc.doc.name, doc.doc.version);
    println!("hash:   {}", doc.hash);
    println!("module: {}", doc.module_id());
    let write = |path: &Option<PathBuf>, content: String| -> Result<()> {
        if let Some(p) = path {
            std::fs::write(p, content).with_context(|| format!("cannot write {}", p.display()))?;
            println!("wrote {}", p.display());
        }
        Ok(())
    };
    write(&args.ts, codegen::typescript(&doc))?;
    write(
        &args.mcp,
        serde_json::to_string_pretty(&codegen::mcp_tools(&doc))? + "\n",
    )?;
    write(&args.docs, codegen::markdown(&doc))?;
    Ok(())
}
