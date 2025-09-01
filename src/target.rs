//! Target-specific configuration that is required for cross-compilation.

use std::{env::consts::{ARCH, OS}, sync::LazyLock};

/// A platform that a Weave application can be compiled to.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Platform {
  /// aarch64-apple-darwin
  AppleAarch64,

  /// aarch64-unknown-linux
  LinuxAarch64,

  /// x86-unknown-linux
  LinuxX86,

  /// x86_64-unknown-linux
  LinuxX86_64,

  /// aarch64-pc-windows
  WindowsAarch64,

  /// x86-pc-windows
  WindowsX86,

  /// x86_64-pc-windows
  WindowsX86_64,
}

impl Platform {
  /// Returns the current platform of the host machine.
  pub fn current() -> Self {
    match (OS, ARCH) {
      ("macos", "aarch64") => Self::AppleAarch64,
      ("linux", "aarch64") => Self::LinuxAarch64,
      ("linux", "x86") => Self::LinuxX86,
      ("linux", "x86_64") => Self::LinuxX86_64,
      ("windows", "x86") => Self::WindowsX86,
      ("windows", "x86_64") => Self::WindowsX86_64,
      ("windows", "aarch64") => Self::WindowsAarch64,
      _ => panic!("Unsupported machine platform."),
    }
  }
}

/// Configuration specific to the compilation target.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TargetConfig {
  /// Platform (OS + architecture) of the target.
  pub platform: Platform,

  /// Size of a pointer, in bytes, for the target architecture.
  pub pointer_size: usize,
}

impl TargetConfig {
  /// Native target configuration.
  pub fn native() -> Self {
    Self {
      platform: Platform::current(),
      pointer_size: size_of::<usize>(),
    }
  }
}

// TODO: Add cross-compilation support.
static TARGET_CONFIG: LazyLock<TargetConfig> = LazyLock::new(||
  TargetConfig::native()
);
