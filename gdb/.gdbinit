add-auto-load-safe-path ~/.gdbinit
set host-charset UTF-8
set target-charset UTF-8
set target-wide-charset UTF-8
set style enabled off
set complaints 0
set confirm off
set history save on
set history filename ~/.gdb_history

# show source tui
define src
  layout src
  layout reg
end

# show assembly tui
define asm
  layout asm
  layout reg
end

# print backtrace w/ rbp
# fallback for when `bt` is broken
# we recommend -fno-omit-frame-pointer
# needed because ape won't use -funwind-tables
define et
  set $x = (void **)$rbp
  while $x
    x/2a $x
    set $x = (void **)$x[0]
  end
end

layout src
