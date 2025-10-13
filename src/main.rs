#![deny(clippy::correctness)]
#![deny(clippy::suspicious)]
#![deny(clippy::perf)]
#![deny(clippy::style)]

use std::fmt;
use std::io::{self, Write};
use std::time::Duration;

use anyhow::Result;
use clap::*;
use indicatif::ProgressBar;
use reqwest::Client;
use serde::de::Error;
use serde::Deserializer;
use serde::{Deserialize, Serialize};
use serde_json::{self, Value};

/// This program fetches definitions of latin words from [https://latin-is-simple.com].
///
/// By default, it searches and fetches, then displays the first definition of the given word.
/// If you input -i, it asks you to select the word out of the results.
#[derive(Parser)]
#[command(name = "Latin dictionary", version, about)]
pub struct Arguments {
    #[arg(short, long)]
    interactive: bool,
    #[arg(short)]
    n: Option<u64>,
    #[arg(long)]
    forms_only: bool,
    #[arg(value_parser=non_empty)]
    query: Option<String>,
    #[arg(default_value_t = LookupType::Whitakers)]
    source: LookupType,
}

#[derive(Clone, Copy, ValueEnum)]
pub enum LookupType {
    LIS,
    Whitakers,
}

impl fmt::Display for LookupType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LookupType::LIS => f.write_str("lis"),
            LookupType::Whitakers => f.write_str("whitakers"),
        }
    }
}

fn non_empty(input: &str) -> Result<String, String> {
    if input.is_empty() {
        Err("Expected a word, instead received an empty string.".into())
    } else {
        Ok(input.into())
    }
}

#[tokio::main]
async fn main() {
    let args = Arguments::parse();

    let client = match Client::builder().build() {
        Ok(c) => c,
        Err(e) => panic!("Failed to create a reqwest client: {:?}", e),
    };

    let search_result = if let Some(q) = &args.query {
        let prog = ProgressBar::new_spinner().with_message(format!("searching for \"{}\"...", q));
        prog.enable_steady_tick(Duration::from_millis(50));
        let result = match args.source {
            LookupType::LIS => search_lis(&client, q, args.forms_only).await.unwrap(),
            LookupType::Whitakers => search_whitakers(&client, q).await.unwrap(),
        };
        prog.finish_and_clear();
        result
    } else if args.interactive {
        read_query_from_stdin()
    } else {
        panic!(
            "You did not specify a word, and you did not specify interactive mode(-i).\nAborting."
        );
    };

    let search_result_parsed: SearchResult = match args.source {
        LookupType::LIS => SearchResult::Lis(serde_json::from_str(&search_result).unwrap_or_else(|err|
            panic!("Received malformed response(???)\nPlease take the logs and report to the devs.\n content: {}\nerror: {}",&search_result,err),
        )),
        LookupType::Whitakers => SearchResult::W(serde_json::from_str(&search_result).unwrap_or_else(|err|
            panic!("Received malformed response(???)\nPlease take the logs and report to the devs.\n content: {}\nerror: {}",&search_result,err),
        )),
    };
    println!("Search result:");
    match search_result_parsed {
        SearchResult::Lis(res) => {
            for i in res {
                println!("{}", i);
            }
        }
        SearchResult::W(res) => {
            for i in res {
                println!("{}", i);
            }
        }
    }
}

fn read_query_from_stdin() -> String {
    loop {
        print!("word: ");
        let _ = io::stdout().flush();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => continue,
            Ok(_) => {
                let trimmed = input.trim();
                if !trimmed.is_empty() {
                    return trimmed.to_string();
                }
            }
            Err(_) => continue,
        }
    }
}

#[derive(Debug)]
pub enum SearchResult {
    Lis(Vec<LisSearchResult>),
    W(Vec<WSearchResult>),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct LisSearchResult {
    id: u64,
    intern_type: String,
    short_name: String,
    full_name: String,
    #[serde(rename = "type")]
    word_type: LisWordType,
    translations_unstructured: LisTranslations,
    url: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct LisWordType {
    name: String,
    label: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct LisTranslations {
    en: String,
    de: String,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct WSearchResult {
    pub root_lines: Vec<WRootLine>,
    #[serde(default)]
    pub record_matches: Vec<WRecordMatch>,
    #[serde(default)]
    pub meanings: Vec<String>,
    #[serde(default)]
    pub search_query: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct WRootLine {
    pub root: String,
    pub part_of_speech: WPartOfSpeech,
    pub version: Option<WVersion>,
    pub gender: Option<WGender>,
    pub kind: Option<WGender>,
    #[serde(default)]
    pub codes: Option<WRootCodes>,
}

#[derive(Serialize, Deserialize, Debug, Default)]
#[serde(default)]
pub struct WRootCodes {
    pub age: Option<WLetterCode>,
    pub area: Option<WLetterCode>,
    pub geo: Option<WLetterCode>,
    pub frequency: Option<WLetterCode>,
    pub source: Option<WLetterCode>,
}

#[derive(Serialize, Deserialize, Debug, Default)]
#[serde(rename_all = "camelCase", default)]
pub struct WRecordMatch {
    #[serde(deserialize_with = "deserialize_option_u8")]
    pub declension: Option<u8>,
    #[serde(deserialize_with = "deserialize_option_u8")]
    pub conjugation: Option<u8>,
    pub tense: Option<WTense>,
    pub voice: Option<WVoice>,
    pub mood: Option<WMood>,
    pub case: Option<WCase>,
    pub number: Option<WNumber>,
    pub gender: Option<WGender>,
    pub comparison: Option<WComparison>,
    #[serde(deserialize_with = "deserialize_option_u8")]
    pub person: Option<u8>,
    pub word_match: Option<String>,
    pub part_of_speech: Option<WPartOfSpeech>,
}

#[derive(ValueEnum, Clone, Copy, Serialize, Deserialize, Debug)]
pub enum WPartOfSpeech {
    PREP,
    VPAR,
    CONJ,
    N,
    X,
    V,
    ADJ,
    ADV,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WVersion {
    #[serde(rename = "1st")]
    First,
    #[serde(rename = "2nd")]
    Second,
    #[serde(rename = "3rd")]
    Third,
    #[serde(rename = "4th")]
    Fourth,
    #[serde(rename = "5th")]
    Fifth,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WGender {
    #[serde(rename = "M")]
    Masculine,
    #[serde(rename = "F")]
    Feminine,
    #[serde(rename = "N")]
    Neuter,
    #[serde(rename = "C")]
    Common,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WTense {
    #[serde(rename = "PRES")]
    Present,
    #[serde(rename = "IMPF")]
    Imperfect,
    #[serde(rename = "FUT")]
    Future,
    #[serde(rename = "PERF")]
    Perfect,
    #[serde(rename = "PLUP")]
    Pluperfect,
    #[serde(rename = "FUTP")]
    FuturePerfect,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WVoice {
    #[serde(rename = "ACTIVE")]
    Active,
    #[serde(rename = "PASSIVE")]
    Passive,
    #[serde(rename = "DEP")]
    Deponent,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WMood {
    #[serde(rename = "IND")]
    Indicative,
    #[serde(rename = "SUB")]
    Subjunctive,
    #[serde(rename = "IMP")]
    Imperative,
    #[serde(rename = "INF")]
    Infinitive,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WCase {
    #[serde(rename = "NOM")]
    Nominative,
    #[serde(rename = "VOC")]
    Vocative,
    #[serde(rename = "ACC")]
    Accusative,
    #[serde(rename = "GEN")]
    Genitive,
    #[serde(rename = "DAT")]
    Dative,
    #[serde(rename = "ABL")]
    Ablative,
    #[serde(rename = "LOC")]
    Locative,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WNumber {
    #[serde(rename = "S")]
    Singular,
    #[serde(rename = "P")]
    Plural,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WComparison {
    #[serde(rename = "POS")]
    Positive,
    #[serde(rename = "COMP")]
    Comparative,
    #[serde(rename = "SUPER")]
    Superlative,
    #[serde(other)]
    Other,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy)]
pub enum WLetterCode {
    #[serde(rename = "A")]
    A,
    #[serde(rename = "B")]
    B,
    #[serde(rename = "C")]
    C,
    #[serde(rename = "D")]
    D,
    #[serde(rename = "E")]
    E,
    #[serde(rename = "F")]
    F,
    #[serde(rename = "G")]
    G,
    #[serde(rename = "H")]
    H,
    #[serde(rename = "I")]
    I,
    #[serde(rename = "J")]
    J,
    #[serde(rename = "K")]
    K,
    #[serde(rename = "L")]
    L,
    #[serde(rename = "M")]
    M,
    #[serde(rename = "N")]
    N,
    #[serde(rename = "O")]
    O,
    #[serde(rename = "P")]
    P,
    #[serde(rename = "Q")]
    Q,
    #[serde(rename = "R")]
    R,
    #[serde(rename = "S")]
    S,
    #[serde(rename = "T")]
    T,
    #[serde(rename = "U")]
    U,
    #[serde(rename = "V")]
    V,
    #[serde(rename = "W")]
    W,
    #[serde(rename = "X")]
    X,
    #[serde(rename = "Y")]
    Y,
    #[serde(rename = "Z")]
    Z,
    #[serde(other)]
    Other,
}

impl fmt::Display for WPartOfSpeech {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WPartOfSpeech::PREP => "preposition",
            WPartOfSpeech::VPAR => "verb participle",
            WPartOfSpeech::CONJ => "conjunction",
            WPartOfSpeech::N => "noun",
            WPartOfSpeech::X => "indeclinable",
            WPartOfSpeech::V => "verb",
            WPartOfSpeech::ADJ => "adjective",
            WPartOfSpeech::ADV => "adverb",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WVersion::First => "1st",
            WVersion::Second => "2nd",
            WVersion::Third => "3rd",
            WVersion::Fourth => "4th",
            WVersion::Fifth => "5th",
            WVersion::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WGender {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WGender::Masculine => "masculine",
            WGender::Feminine => "feminine",
            WGender::Neuter => "neuter",
            WGender::Common => "common",
            WGender::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WTense {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WTense::Present => "present",
            WTense::Imperfect => "imperfect",
            WTense::Future => "future",
            WTense::Perfect => "perfect",
            WTense::Pluperfect => "pluperfect",
            WTense::FuturePerfect => "future perfect",
            WTense::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WVoice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WVoice::Active => "active",
            WVoice::Passive => "passive",
            WVoice::Deponent => "deponent",
            WVoice::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WMood {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WMood::Indicative => "indicative",
            WMood::Subjunctive => "subjunctive",
            WMood::Imperative => "imperative",
            WMood::Infinitive => "infinitive",
            WMood::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WCase::Nominative => "nominative",
            WCase::Vocative => "vocative",
            WCase::Accusative => "accusative",
            WCase::Genitive => "genitive",
            WCase::Dative => "dative",
            WCase::Ablative => "ablative",
            WCase::Locative => "locative",
            WCase::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WNumber::Singular => "singular",
            WNumber::Plural => "plural",
            WNumber::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WComparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WComparison::Positive => "positive",
            WComparison::Comparative => "comparative",
            WComparison::Superlative => "superlative",
            WComparison::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for WLetterCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            WLetterCode::A => "A",
            WLetterCode::B => "B",
            WLetterCode::C => "C",
            WLetterCode::D => "D",
            WLetterCode::E => "E",
            WLetterCode::F => "F",
            WLetterCode::G => "G",
            WLetterCode::H => "H",
            WLetterCode::I => "I",
            WLetterCode::J => "J",
            WLetterCode::K => "K",
            WLetterCode::L => "L",
            WLetterCode::M => "M",
            WLetterCode::N => "N",
            WLetterCode::O => "O",
            WLetterCode::P => "P",
            WLetterCode::Q => "Q",
            WLetterCode::R => "R",
            WLetterCode::S => "S",
            WLetterCode::T => "T",
            WLetterCode::U => "U",
            WLetterCode::V => "V",
            WLetterCode::W => "W",
            WLetterCode::X => "X",
            WLetterCode::Y => "Y",
            WLetterCode::Z => "Z",
            WLetterCode::Other => "other",
        };
        f.write_str(text)
    }
}

impl fmt::Display for LisWordType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.label.eq_ignore_ascii_case(&self.name) {
            write!(f, "{}", self.label)
        } else {
            write!(f, "{} ({})", self.label, self.name)
        }
    }
}

impl fmt::Display for LisTranslations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        let en = self.en.trim();
        if !en.is_empty() {
            parts.push(format!("en: {}", en));
        }
        let de = self.de.trim();
        if !de.is_empty() {
            parts.push(format!("de: {}", de));
        }

        if parts.is_empty() {
            f.write_str("—")
        } else {
            f.write_str(&parts.join(" | "))
        }
    }
}

impl fmt::Display for LisSearchResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut lines = Vec::new();
        lines.push(format!("{} — {}", self.short_name, self.word_type));
        if self.full_name != self.short_name {
            lines.push(format!("Full entry: {}", self.full_name));
        }
        lines.push(format!("Gloss: {}", self.translations_unstructured));
        lines.push(format!("Link: {}", self.url));
        f.write_str(&lines.join("\n"))
    }
}

impl fmt::Display for WRootCodes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        if let Some(age) = self.age {
            parts.push(format!("age={}", age));
        }
        if let Some(area) = self.area {
            parts.push(format!("area={}", area));
        }
        if let Some(geo) = self.geo {
            parts.push(format!("geo={}", geo));
        }
        if let Some(freq) = self.frequency {
            parts.push(format!("frequency={}", freq));
        }
        if let Some(source) = self.source {
            parts.push(format!("source={}", source));
        }

        f.write_str(&parts.join(", "))
    }
}

impl fmt::Display for WRootLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut details = Vec::new();
        if let Some(version) = self.version {
            details.push(format!("version {}", version));
        }
        if let Some(gender) = self.gender {
            details.push(format!("gender {}", gender));
        }
        if let Some(kind) = self.kind {
            details.push(format!("kind {}", kind));
        }
        if let Some(codes) = &self.codes {
            let rendered = codes.to_string();
            if !rendered.is_empty() {
                details.push(format!("codes {{{}}}", rendered));
            }
        }

        if details.is_empty() {
            write!(f, "{} — {}", self.root.trim(), self.part_of_speech)
        } else {
            write!(
                f,
                "{} — {} ({})",
                self.root.trim(),
                self.part_of_speech,
                details.join(", ")
            )
        }
    }
}

impl fmt::Display for WRecordMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        if let Some(word) = &self.word_match {
            parts.push(format!("form \"{}\"", word));
        }
        if let Some(pos) = self.part_of_speech {
            parts.push(format!("{}", pos));
        }
        if let Some(decl) = self.declension {
            parts.push(format!("declension {}", decl));
        }
        if let Some(conj) = self.conjugation {
            parts.push(format!("conjugation {}", conj));
        }
        if let Some(person) = self.person {
            parts.push(format!("person {}", person));
        }
        if let Some(tense) = self.tense {
            parts.push(format!("tense {}", tense));
        }
        if let Some(voice) = self.voice {
            parts.push(format!("voice {}", voice));
        }
        if let Some(mood) = self.mood {
            parts.push(format!("mood {}", mood));
        }
        if let Some(case_) = self.case {
            parts.push(format!("case {}", case_));
        }
        if let Some(number) = self.number {
            parts.push(format!("number {}", number));
        }
        if let Some(gender) = self.gender {
            parts.push(format!("gender {}", gender));
        }
        if let Some(comparison) = self.comparison {
            parts.push(format!("comparison {}", comparison));
        }

        f.write_str(&parts.join(", "))
    }
}

impl fmt::Display for WSearchResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sections = Vec::new();

        if let Some(query) = &self.search_query {
            let trimmed = query.trim();
            if !trimmed.is_empty() {
                sections.push(format!("Query: {}", trimmed));
            }
        }

        if !self.root_lines.is_empty() {
            sections.push("Roots:".to_string());
            for root in &self.root_lines {
                sections.push(format!("  - {}", root));
            }
        }

        if !self.record_matches.is_empty() {
            sections.push("Forms:".to_string());
            for record in &self.record_matches {
                sections.push(format!("  - {}", record));
            }
        }

        let meanings: Vec<String> = self
            .meanings
            .iter()
            .map(|m| m.trim())
            .filter(|m| !m.is_empty())
            .map(|m| m.to_string())
            .collect();
        if !meanings.is_empty() {
            sections.push(format!("Meanings: {}", meanings.join("; ")));
        }

        f.write_str(&sections.join("\n"))
    }
}

fn deserialize_option_u8<'de, D>(deserializer: D) -> Result<Option<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    let value = Option::<Value>::deserialize(deserializer)?;
    match value {
        None => Ok(None),
        Some(Value::Null) => Ok(None),
        Some(Value::Number(num)) => num
            .as_u64()
            .and_then(|v| u8::try_from(v).ok())
            .map(Some)
            .ok_or_else(|| D::Error::custom("expected u8-compatible number")),
        Some(Value::String(s)) => {
            let trimmed = s.trim();
            if trimmed.eq_ignore_ascii_case("x") {
                Ok(None)
            } else {
                trimmed
                    .parse::<u8>()
                    .map(Some)
                    .map_err(|_| D::Error::custom("expected u8-compatible string"))
            }
        }
        Some(other) => Err(D::Error::custom(format!(
            "unexpected type for numeric field: {:?}",
            other
        ))),
    }
}

async fn search_lis(client: &Client, input: &str, forms_only: bool) -> Result<String> {
    let req = client
        .get("https://latin-is-simple.com/api/vocabulary/search/")
        .query(&[("query", input), ("forms_only", &forms_only.to_string())])
        .build()
        .unwrap();
    let response = client.execute(req).await?;
    Ok(response.text().await?)
}

async fn search_whitakers(client: &Client, input: &str) -> Result<String> {
    let req = client
        .get(format!(
            "https://whitakers-words.com/api/translate/latin/{}",
            input
        ))
        .build()
        .unwrap();
    let response = client.execute(req).await?;
    Ok(response.text().await?)
}
