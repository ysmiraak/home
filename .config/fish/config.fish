set -g -x EMAIL ysmiraak@gmail.com

set -g -x LANG en_US.UTF-8
set -g -x LC_ALL en_US.UTF-8
set -g -x PATH /usr/local/bin /usr/local/sbin /Users/YK/.cabal/bin /Users/YK/.cargo/bin $PATH
set -g -x RUST_SRC_PATH ~/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src

set -g -x TERM xterm-256color
set -g -x ARCHFLAGS "-arch x86_64"