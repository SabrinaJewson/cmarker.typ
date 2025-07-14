fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    generate_entities(&out_dir);
}

/// Produces a PHF hash map from `entities.json`. The file was obtained using the comment:
///
/// ```sh
/// curl -O https://html.spec.whatwg.org/entities.json
/// ```
fn generate_entities(out_dir: &Path) {
    let json = fs::read("entities.json").unwrap();
    let entities = serde_json::from_slice::<HashMap<String, Entity>>(&json).unwrap();

    let mut map = phf_codegen::Map::new();
    for (name, entity) in &entities {
        let name = name.strip_prefix("&").unwrap();
        let Some(name) = name.strip_suffix(";") else {
            continue;
        };
        let value = match &*entity.characters {
            "\"" => "\\\"",
            "\\" => "\\\\",
            other => other,
        };
        let value = format!("\"{}\"", value.escape_default());
        map.entry(name.as_bytes(), value);
    }
    let code = format!(
        "pub(crate) const ENTITIES: phf::Map<&[u8], &str> = {};\n",
        map.build()
    );

    fs::write(out_dir.join("entities.rs"), &code).unwrap();
}

#[derive(Deserialize)]
struct Entity {
    characters: String,
}

use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
