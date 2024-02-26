const std = @import("std");

pub const vm = @import("./vm.zig");

pub fn main() !void {
    var machine = vm.VirtualMachine.init(&.{1, 0, 0, 0}, &.{});
    machine.run();
}

test {
    std.testing.refAllDecls(@This());
}
