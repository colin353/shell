//! The shell multiplexer, split into the standalone process and the
//! daemon/client halves. The binary (`main.rs`) is a thin dispatcher over these
//! modules; they live in the library so integration tests can drive the daemon
//! directly.

pub mod client;
pub mod common;
pub mod server;
pub mod standalone;
