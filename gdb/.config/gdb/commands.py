import os
import gdb


class SetupRustPaths(gdb.Command):
    """Setup Rust source path mappings using environment variables.
    Usage: ht-rust-paths
    Uses RUSTC_COMMIT_HASH and RUSTC_SYSROOT environment variables to set up source path mapping.
    """

    def __init__(self):
        super().__init__("ht-rust-paths", gdb.COMMAND_FILES)

    def invoke(self, args, from_tty):
        commit_hash = os.getenv("RUSTC_COMMIT_HASH")
        sysroot = os.getenv("RUSTC_SYSROOT")

        if not commit_hash or not sysroot:
            print("Error: Required environment variables not set")
            print("Please set both RUSTC_COMMIT_HASH and RUSTC_SYSROOT")
            return

        rust_path = f"/rustc/{commit_hash}"
        rust_src = f"{sysroot}/lib/rustlib/src/rust"

        gdb.execute(f"set substitute-path {rust_path} {rust_src}")
        print(f"Set up Rust source mapping:")
        print(f"  {rust_path} -> {rust_src}")


# Register the command
SetupRustPaths()
