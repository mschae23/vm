const std = @import("std");

pub const vm = @import("./vm.zig");

pub fn main() !void {
    var machine = vm.VirtualMachine.init(&.{
        4, 0, 20, 0, // LOAD INT $0 20
        4, 1, 5, 0,  // LOAD INT $1 5
        14, 0, 1, 0, // ADDI $0 $1 $0
        3, 0, 0, 0,  // DBG $0
    }, &.{
    });
    machine.run();
}

test {
    std.testing.refAllDecls(@This());
}
