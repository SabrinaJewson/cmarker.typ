fn main() -> anyhow::Result<()> {
    env::set_current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/.."))?;
    let bless = env::var_os("BLESS").is_some();

    let e = process::Command::new("cargo")
        .args(["build", "-p", "plugin"])
        .arg("--no-default-features")
        .args(["--target", "wasm32v1-none"])
        .status()
        .context("running cargo build")?;
    anyhow::ensure!(e.success(), "cargo build failed");

    fs::copy(
        "target/wasm32v1-none/debug/plugin.wasm",
        "plugin.wasm",
    )
    .context("copying plugin.wasm")?;

    let mut entries = fs::read_dir("tests")
        .context("reading tests dir")?
        .map(|entry| entry.context("entry in tests dir").map(|e| e.path()))
        .collect::<anyhow::Result<Vec<_>>>()?;

    entries.retain(|e| e.extension() == Some("md".as_ref()));

    let errors = entries
        .par_iter()
        .map(|md_path| -> anyhow::Result<String> {
            match run_test(md_path, bless).with_context(|| md_path.display().to_string())? {
                Ok(()) => Ok(String::new()),
                Err(e) => Ok(e),
            }
        })
        .try_reduce(String::new, |mut l, r| {
            l.push_str(&r);
            Ok(l)
        })?;

    if !errors.is_empty() {
        println!("{errors}");
        anyhow::bail!("Exiting due to above errors");
    }

    Ok(())
}

fn run_test(md_path: &Path, bless: bool) -> anyhow::Result<Result<(), String>> {
    let html_path = md_path.with_extension("html");

    let typst_command = || {
        let mut command = process::Command::new("typst");
        command.arg("--color=always");
        command.args(["compile", "test-runner/scaffold.typ", "-"]);
        command.arg("--format=html");
        command.arg("--features=html");
        command.arg("--root=.");
        command.arg(format!("--input=md=../{}", md_path.display()));
        command
    };
    let output = typst_command().output().context("running typst")?;

    let stderr = String::from_utf8(output.stderr).context("stderr not utf-8")?;

    if !output.status.success() {
        let mut msg = format!(
            "\n{}\n",
            format_args!("{}{}: Typst errored", "Error: ".red(), md_path.display()).bold(),
        );
        for line in stderr.lines() {
            msg.push_str("\n    ");
            msg.push_str(line);
        }

        let output = typst_command()
            .arg("--input=show-source=")
            .output()
            .context("running typst again")?;
        if !output.stdout.is_empty() {
            let stdout = String::from_utf8(output.stdout).context("stdout not utf-8")?;
            if let Some((hex, _)) = stdout
                .split_once("SOURCESTART")
                .and_then(|(_, rest)| rest.split_once("SOURCEEND"))
            {
                let bytes = hex
                    .as_bytes()
                    .chunks_exact(2)
                    .map(|chunk| {
                        ((char::from(chunk[0]).to_digit(16).unwrap() as u8) << 4)
                            | char::from(chunk[1]).to_digit(16).unwrap() as u8
                    })
                    .collect::<Vec<u8>>();

                writeln!(
                    msg,
                    "\n  {}",
                    format_args!("{} raw source code", "Note:".white()).bold()
                )
                .unwrap();

                let s = String::from_utf8(bytes).unwrap();
                for line in s.lines() {
                    msg.push_str("\n    ");
                    msg.push_str(line);
                }
            }
        }

        return Ok(Err(msg));
    }

    let html = String::from_utf8(output.stdout).context("stdout not utf-8")?;

    if !fs::exists(&html_path)? || bless {
        println!("{} {}", "Writing".bold().blue(), html_path.display());
        fs::write(&html_path, html).context("writing HTML")?;
        return Ok(Ok(()));
    }

    let old = fs::read_to_string(&html_path).context("reading HTML")?;

    if old == html {
        println!("{} {}", "Success".green().bold(), md_path.display());
        return Ok(Ok(()));
    }

    let config = prettydiff::text::ContextConfig {
        context_size: 3,
        skipping_marker: &"~~~".white().to_string(),
    };
    let diff = prettydiff::text::diff_lines(&old, &html).format_with_context(Some(config), true);
    let msg = format!(
        "\n{}\n{diff}\n{}Run with BLESS=1 to overwrite\n",
        format_args!(
            "{}{} differs from current output",
            "Error: ".red(),
            html_path.display()
        )
        .bold(),
        "Note: ".white(),
    );

    Ok(Err(msg))
}

use anyhow::Context;
use owo_colors::OwoColorize as _;
use rayon::iter::IntoParallelRefIterator as _;
use rayon::iter::ParallelIterator as _;
use std::env;
use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process;
