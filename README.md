# widget

A js2wasm command example written in PureScript.

## About

This project does nothing useful. The goal is to start learning PureScript by writing a non-trivial
`js2wasm` Wasm command.

This demonstrates executing business logic – without side effects – in a sandboxed environment. One
could imagine a workflow orchestration layer in front of a set of `js2wasm` commands. This would
allow creating complex workflows with simple components. These commands can easily be integration
tested via command line.

