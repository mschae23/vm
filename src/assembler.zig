const std = @import("std");
const vm = @import("./main.zig").vm;
const Opcode = vm.Opcode;

pub const Assembly = struct {
    instructions: []u8,
    constants: []vm.Value,
};

pub const OpcodeItem = struct {
    name: [:0]const u8,
    opcode: Opcode,
};

pub const OPCODE_TABLE: []const OpcodeItem = &.{
    OpcodeItem { .name = "nop", .opcode = Opcode.nop, },
    OpcodeItem { .name = "halt", .opcode = Opcode.halt, },
    // OpcodeItem { .name = "igl", .opcode = Opcode.igl, },

    OpcodeItem { .name = "dbg", .opcode = Opcode.dbg, },

    OpcodeItem { .name = "load_int", .opcode = Opcode.load_int, },
    OpcodeItem { .name = "load_true", .opcode = Opcode.load_true, },
    OpcodeItem { .name = "load_false", .opcode = Opcode.load_false, },
    OpcodeItem { .name = "load_const", .opcode = Opcode.load_const, },

    OpcodeItem { .name = "mov", .opcode = Opcode.mov, },
    OpcodeItem { .name = "convitof", .opcode = Opcode.convitof, },
    OpcodeItem { .name = "convitob", .opcode = Opcode.convitob, },
    OpcodeItem { .name = "convftoi", .opcode = Opcode.convftoi, },
    OpcodeItem { .name = "convbtoi", .opcode = Opcode.convbtoi, },
    OpcodeItem { .name = "clear", .opcode = Opcode.clear, },

    OpcodeItem { .name = "addi", .opcode = Opcode.addi, },
    OpcodeItem { .name = "subi", .opcode = Opcode.subi, },
    OpcodeItem { .name = "muli", .opcode = Opcode.muli, },
    OpcodeItem { .name = "divi", .opcode = Opcode.divi, },
    OpcodeItem { .name = "modi", .opcode = Opcode.modi, },
    OpcodeItem { .name = "negi", .opcode = Opcode.negi, },
    OpcodeItem { .name = "addf", .opcode = Opcode.addf, },
    OpcodeItem { .name = "subf", .opcode = Opcode.subf, },
    OpcodeItem { .name = "mulf", .opcode = Opcode.mulf, },
    OpcodeItem { .name = "divf", .opcode = Opcode.divf, },
    OpcodeItem { .name = "modf", .opcode = Opcode.modf, },
    OpcodeItem { .name = "negf", .opcode = Opcode.negf, },
    OpcodeItem { .name = "addb", .opcode = Opcode.addb, },
    OpcodeItem { .name = "negb", .opcode = Opcode.negb, },

    OpcodeItem { .name = "jmp", .opcode = Opcode.jmp, },
    OpcodeItem { .name = "jmpf", .opcode = Opcode.jmpf, },
    OpcodeItem { .name = "jmpb", .opcode = Opcode.jmpb, },
    OpcodeItem { .name = "jez", .opcode = Opcode.jez, },
    OpcodeItem { .name = "jnz", .opcode = Opcode.jnz, },

    OpcodeItem { .name = "eqi", .opcode = Opcode.eqi, },
    OpcodeItem { .name = "neqi", .opcode = Opcode.neqi, },
    OpcodeItem { .name = "gti", .opcode = Opcode.gti, },
    OpcodeItem { .name = "lti", .opcode = Opcode.lti, },
    OpcodeItem { .name = "gtqi", .opcode = Opcode.gtqi, },
    OpcodeItem { .name = "ltqi", .opcode = Opcode.ltqi, },
    OpcodeItem { .name = "eqf", .opcode = Opcode.eqf, },
    OpcodeItem { .name = "neqf", .opcode = Opcode.neqf, },
    OpcodeItem { .name = "gtf", .opcode = Opcode.gtf, },
    OpcodeItem { .name = "ltf", .opcode = Opcode.ltf, },
    OpcodeItem { .name = "gtqf", .opcode = Opcode.gtqf, },
    OpcodeItem { .name = "ltqf", .opcode = Opcode.ltqf, },
    OpcodeItem { .name = "eqb", .opcode = Opcode.eqb, },
    OpcodeItem { .name = "neqb", .opcode = Opcode.neqb, },
};

pub const SyntaxError = error { Syntax, };

pub const Assembler = struct {
    allocator: std.mem.Allocator,
    reader: std.fs.File.Reader,

    instructions: std.ArrayListUnmanaged(u8),
    constants: std.ArrayListUnmanaged(vm.Value),

    /// Input has to be ASCII text.
    pub fn init(allocator: std.mem.Allocator, reader: std.fs.File.Reader) std.mem.Allocator.Error!Assembler {
        return Assembler {
            .allocator = allocator,
            .reader = reader,
            .instructions = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 0),
            .constants = try std.ArrayListUnmanaged(vm.Value).initCapacity(allocator, 0),
        };
    }

    pub fn assemble(self: *Assembler) !Assembly {
        var buf: [256]u8 = .{undefined} ** 256;
        var fixedBufferStream = std.io.fixedBufferStream(&buf);
        const writer = fixedBufferStream.writer();

        while (true) {
            self.reader.streamUntilDelimiter(writer, '\n', 256) catch |err| switch (err) {
                error.EndOfStream => if (fixedBufferStream.pos == 0) {
                    return try self.createAssembly();
                },

                else => |e| return e,
            };
            const line = std.mem.trim(u8, fixedBufferStream.getWritten(), std.ascii.whitespace);
            const line_lowered = try std.ascii.allocLowerString(self.allocator, line);
            defer self.allocator.free(line_lowered);

            var opcode: ?Opcode = null;
            var rest = line;

            for (OPCODE_TABLE) |item| {
                if (std.mem.startsWith(u8, line_lowered, item.name)) {
                    opcode = item.opcode;
                    rest = rest[item.name.len..];
                    break;
                }
            }

            if (opcode) |o| {
                switch (o) {
                    .nop => {
                        const slice = try self.instructions.addManyAsArray(self.allocator, 4);
                        slice.* = .{@intFromEnum(o), 0, 0, 0, };
                    },
                    .halt => {
                        const slice = try self.instructions.addManyAsArray(self.allocator, 4);
                        slice.* = .{@intFromEnum(o), 0, 0, 0, };
                    },
                    .igl => {
                        std.debug.print("Illegal instruction `IGL` must not be used.\n", .{});
                        return error.Illegal;
                    },

                    .dbg => self.readDbg(),
                    .load_int => {},
                    .load_true => {},
                    .load_false => {},
                    .load_const => {},

                    .mov => {},
                    .convitof => {},
                    .convitob => {},
                    .convftoi => {},
                    .convbtoi => {},
                    .clear => {},

                    .addi => {},
                    .subi => {},
                    .muli => {},
                    .divi => {},
                    .modi => {},
                    .negi => {},

                    .addf => {},
                    .subf => {},
                    .mulf => {},
                    .divf => {},
                    .modf => {},
                    .negf => {},

                    .addb => {},
                    .negb => {},

                    .jmp => {},
                    .jmpf => {},
                    .jmpb => {},
                    .jez => {},
                    .jnz => {},

                    .eqi => {},
                    .neqi => {},
                    .gti => {},
                    .lti => {},
                    .gtqi => {},
                    .ltqi => {},

                    .eqf => {},
                    .neqf => {},
                    .gtf => {},
                    .ltf => {},
                    .gtqf => {},
                    .ltqf => {},

                    .eqb => {},
                    .neqb => {},
                }
            } else {
                std.debug.print("TODO", .{});
            }

            fixedBufferStream.pos = 0;
        }
    }

    fn createAssembly(self: *Assembler) std.mem.Allocator.Error!Assembly {
        return Assembly {
            .instructions = try self.instructions.toOwnedSlice(self.allocator),
            .constants = try self.constants.toOwnedSlice(self.allocator),
        };
    }

    fn readRegister(self: *Assembler) (std.fs.File.Reader.NoEofError || SyntaxError)!u8 {
        if (try self.reader.readByte() != '$') {
            return error.Syntax;
        }

        var number: u8 = 0;

        while (self.reader.readByte()) |byte| {
            number <<= 4;

            if (byte >= '0' and byte <= '9') {
                number += byte - '0';
            } else if (byte >= 'a' and byte <= 'f') {
                number += byte - 'a' + 10;
            } else if (byte >= 'A' and byte <= 'F') {
                number += byte - 'A' + 10;
            } else if (std.ascii.isWhitespace(byte)) {
                return number;
            } else {
                return error.Syntax;
            }
        }

        return number;
    }

    fn readInteger(self: *Assembler) (std.fs.File.Reader.NoEofError || SyntaxError)!i16 {
        var number: u16 = 0;
        var first = true;
        var sign = false;

        while (self.reader.readByte()) |byte| {
            number *= 10;

            if (first and byte == '-') {
                sign = true;
            } else if (byte >= '0' and byte <= '9') {
                number += byte - '0';
            } else if (std.ascii.isWhitespace(byte)) {
                return number;
            } else {
                return error.Syntax;
            }

            first = false;
        }

        return number;
    }

    /// Caller owns returned memory.
    fn readName(self: *Assembler) (std.mem.Allocator.Error || std.fs.File.Reader.NoEofError || SyntaxError)![:0]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        errdefer list.deinit();

        var first = true;

        while (self.reader.readByte()) |byte| {
            if (byte >= 'a' and byte <= 'z' or byte >= 'A' and byte <= 'Z' or byte == '_') {
                try list.append(byte);
            } else if (first and byte >= '0' and byte <= '9') {
                try list.append(byte);
            } else if (std.ascii.isWhitespace(byte)) {
                return try list.toOwnedSliceSentinel(0);
            } else {
                return error.Syntax;
            }

            first = false;
        }

        return try list.toOwnedSliceSentinel(0);
    }

    fn readDbg(self: *Assembler) (std.fs.File.Reader.NoEofError || SyntaxError)!void {
        _ = self;
        // self.reader.skipUntil(!isWhitespace);
    }
};
