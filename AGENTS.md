# Testing

When testing, use `cargo test -p <crate_name>`.

Avoid running the tests in ALL crates because that is very slow. Instead, pick a subset of relevant crates and run those.

Note that the libvim tests are very slow and tend to be flaky. If they fail, try rerunning specific failing tests.
