const std = @import("std");
const zigargs = @import("zigargs");

pub const vm = @import("./vm.zig");
pub const assembler = @import("./assembler.zig");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const Options = struct {
        help: bool = false,
        version: bool = false,

        pub const shorthands = .{
            .h = "help",
            .v = "version",
        };

        pub const meta = .{
            .option_docs = .{
                .help = "Print this message and exit",
                .version = "Print version",
            }
        };
    };

    const options = zigargs.parseForCurrentProcess(Options, allocator, .print) catch return 1;
    defer options.deinit();

    if (options.options.help) {
        try zigargs.printHelp(Options, options.executable_name orelse "vm", std.io.getStdOut().writer());
        return 0;
    } else if (options.options.version) {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s} {s}\n", .{options.executable_name orelse "vm", "1.0.0", });
        return 0;
    }

    if (options.positionals.len != 1) {
        std.debug.print("No file name provided.\n\n", .{});
        try zigargs.printHelp(Options, options.executable_name orelse "vm", std.io.getStdOut().writer());

        return 64; // USAGE
    }

    const file_path = options.positionals[0];
    var file = if (std.mem.eql(u8, file_path, "-")) std.io.getStdIn() else try std.fs.cwd().openFile(file_path, .{});
    defer file.close();
    const reader = file.reader();

    const assembly = a: {
        var a = try assembler.Assembler.init(allocator, reader);
        defer a.deinit();
        break :a try a.assemble();
    };
    defer allocator.free(assembly.instructions);
    defer allocator.free(assembly.constants);

    var machine = vm.VirtualMachine.init(assembly.instructions, assembly.constants);
    machine.run();

    return 0;
}

test {
    std.testing.refAllDecls(@This());
}
