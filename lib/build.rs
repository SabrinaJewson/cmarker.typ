fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    entities::generate(&out_dir);
    github_slug_disallow::generate(&out_dir);
}

mod entities {
    /// Produces a PHF hash map from `entities.json`. The file was obtained using the comment:
    ///
    /// ```sh
    /// curl -O https://html.spec.whatwg.org/entities.json
    /// ```
    pub(crate) fn generate(out_dir: &Path) {
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
    use std::fs;
    use std::path::Path;
}

mod github_slug_disallow {
    /// Generate the trie of disallowed characters in GitHub slugs. Logic copied from:
    /// <https://github.com/Flet/github-slugger/blob/3461c4350868329c8530904d170358bca1d31448/script/generate-regex.js>
    pub(crate) fn generate(out_dir: &Path) {
        let categories = GeneralCategoryGroup::empty()
            .union(GeneralCategoryGroup::OtherNumber)
            .union(GeneralCategoryGroup::ClosePunctuation)
            .union(GeneralCategoryGroup::FinalPunctuation)
            .union(GeneralCategoryGroup::InitialPunctuation)
            .union(GeneralCategoryGroup::OpenPunctuation)
            .union(GeneralCategoryGroup::OtherPunctuation)
            .union(GeneralCategoryGroup::DashPunctuation)
            .union(GeneralCategoryGroup::Symbol)
            .union(GeneralCategoryGroup::Control)
            .union(GeneralCategoryGroup::PrivateUse)
            .union(GeneralCategoryGroup::Format)
            .union(GeneralCategoryGroup::Unassigned)
            .union(GeneralCategoryGroup::Separator);

        let alphabetics = CodePointSetData::new::<Alphabetic>();

        let to_remove = <CodePointMapData<GeneralCategory>>::new()
            .iter_ranges_for_group(categories)
            .flatten()
            .filter(|&codepoint| {
                !alphabetics.contains32(codepoint)
                    && codepoint != ' ' as u32
                    && codepoint != '-' as u32
            });

        let trie = TrieSetOwned::from_codepoints(to_remove).unwrap();
        let trie = trie.as_slice();

        // This is probably not public API, but I see no way to achieve it otherwise. Taken from:
        // <https://github.com/BurntSushi/ucd-generate/blob/e8fa937f0cac643669dcaf5edac2785b15cab917/src/writer.rs#L206>
        let mut out = "pub(crate) const SET: &ucd_trie::TrieSet = &ucd_trie::TrieSet{".to_owned();
        macro_rules! field {
            ($field:ident, $writer:ident) => {
                out.push_str(concat!(stringify!($field), ":"));
                $writer(trie.$field, &mut out);
                out.push(',');
            };
        }
        field!(tree1_level1, write_slice_u64);
        field!(tree2_level1, write_slice_u8);
        field!(tree2_level2, write_slice_u64);
        field!(tree3_level1, write_slice_u8);
        field!(tree3_level2, write_slice_u8);
        field!(tree3_level3, write_slice_u64);

        out.push_str("};");

        fs::write(out_dir.join("github_slug_disallow.rs"), &out).unwrap();
    }

    fn write_slice_u64(slice: &[u64], out: &mut String) {
        out.push_str("&[");
        for &n in slice {
            match n {
                0 => out.push_str("0,"),
                n if n.ilog10() < n.ilog2() / 4 + 2 => write!(out, "{n},").unwrap(),
                n => write!(out, "0x{n:x},").unwrap(),
            }
        }
        out.pop();
        out.push(']');
    }

    fn write_slice_u8(slice: &[u8], out: &mut String) {
        out.push_str("b\"");
        for &n in slice {
            out.push_str("\\x");
            write!(out, "{n:02x}").unwrap();
        }
        out.push('"');
    }

    use icu_properties::CodePointMapData;
    use icu_properties::CodePointSetData;
    use icu_properties::props::Alphabetic;
    use icu_properties::props::GeneralCategory;
    use icu_properties::props::GeneralCategoryGroup;
    use std::fmt::Write as _;
    use std::fs;
    use std::path::Path;
    use ucd_trie::TrieSetOwned;
}

use std::env;
use std::path::PathBuf;
