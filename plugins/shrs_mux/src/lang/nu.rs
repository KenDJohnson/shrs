use std::process::{Command, Stdio};

use shrs::prelude::*;

pub struct NuLang {}

impl NuLang {
    pub fn new() -> Self {
        Self {}
    }
}

impl Lang for NuLang {
    fn eval(
        &self,
        _sh: &Shell,
        _ctx: &mut Context,
        _rt: &mut Runtime,
        cmd: String,
    ) -> shrs::anyhow::Result<CmdOutput> {
        let handle = Command::new("nu")
            .args(vec!["-c", &cmd])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let _output = handle.wait_with_output()?;
        // ctx.out.print(output.stdout);

        Ok(CmdOutput::success())
    }

    fn name(&self) -> String {
        "nu".to_string()
    }

    fn needs_line_check(&self, _cmd: String) -> bool {
        false
    }
}
