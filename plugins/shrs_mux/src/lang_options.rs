use std::collections::HashMap;

use shrs::{
    anyhow,
    prelude::{Context, Highlighter, Runtime, Shell},
};

use crate::ChangeLangCtx;

#[allow(dead_code)]
pub struct LangOptions {
    highlighters: HashMap<String, Box<dyn Highlighter>>,
}
impl LangOptions {
    pub fn new(highlighters: HashMap<String, Box<dyn Highlighter>>) -> Self {
        LangOptions { highlighters }
    }
}
impl Default for LangOptions {
    fn default() -> Self {
        let highlighters: HashMap<String, Box<dyn Highlighter>> = HashMap::from([]);
        Self { highlighters }
    }
}
pub(crate) fn swap_lang_options(
    _sh: &Shell,
    _sh_ctx: &mut Context,
    _sh_rt: &mut Runtime,
    _ctx: &ChangeLangCtx,
) -> anyhow::Result<()> {
    Ok(())
}
