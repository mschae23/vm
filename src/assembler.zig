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

    OpcodeItem { .name = "load int", .opcode = Opcode.load_int, },
    OpcodeItem { .name = "load true", .opcode = Opcode.load_true, },
    OpcodeItem { .name = "load false", .opcode = Opcode.load_false, },
    OpcodeItem { .name = "load const", .opcode = Opcode.load_const, },

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
            const line = std.mem.trim(u8, fixedBufferStream.getWritten(), &std.ascii.whitespace);

            if (std.mem.startsWith(u8, line, "#")) {
                continue;
            }

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

            var fixedBufferStream2 = std.io.fixedBufferStream(rest);
            const line_reader = fixedBufferStream2.reader().any();

            if (opcode) |o| {
                switch (o) {
                    .nop => try self.addInstruction(o, 0, 0, 0),
                    .halt => try self.addInstruction(o, 0, 0, 0),
                    .igl => {
                        std.debug.print("Illegal instruction `IGL` must not be used.\n", .{});
                        return error.Illegal;
                    },

                    .dbg => try self.readDbg(line_reader),
                    .load_int => try self.readLoadInt(line_reader),
                    .load_true => try self.readOnlyRegister1(line_reader, o),
                    .load_false => try self.readOnlyRegister1(line_reader, o),
                    .load_const => {
                        std.debug.print("Instruction `LOAD CONST` is not yet implemented.\n", .{});
                        return error.Illegal;
                    },

                    .mov => try self.readOnlyRegister2(line_reader, o),
                    .convitof => try self.readOnlyRegister2(line_reader, o),
                    .convitob => try self.readOnlyRegister2(line_reader, o),
                    .convftoi => try self.readOnlyRegister2(line_reader, o),
                    .convbtoi => try self.readOnlyRegister2(line_reader, o),
                    .clear => try self.readOnlyRegister1(line_reader, o),

                    .addi => try self.readOnlyRegister3(line_reader, o),
                    .subi => try self.readOnlyRegister3(line_reader, o),
                    .muli => try self.readOnlyRegister3(line_reader, o),
                    .divi => try self.readOnlyRegister3(line_reader, o),
                    .modi => try self.readOnlyRegister3(line_reader, o),
                    .negi => try self.readOnlyRegister2(line_reader, o),

                    .addf => try self.readOnlyRegister3(line_reader, o),
                    .subf => try self.readOnlyRegister3(line_reader, o),
                    .mulf => try self.readOnlyRegister3(line_reader, o),
                    .divf => try self.readOnlyRegister3(line_reader, o),
                    .modf => try self.readOnlyRegister3(line_reader, o),
                    .negf => try self.readOnlyRegister2(line_reader, o),

                    .addb => try self.readOnlyRegister3(line_reader, o),
                    .negb => try self.readOnlyRegister2(line_reader, o),

                    .jmp => {},
                    .jmpf => {},
                    .jmpb => {},
                    .jez => {},
                    .jnz => {},

                    .eqi =>  try self.readOnlyRegister3(line_reader, o),
                    .neqi => try self.readOnlyRegister3(line_reader, o),
                    .gti =>  try self.readOnlyRegister3(line_reader, o),
                    .lti =>  try self.readOnlyRegister3(line_reader, o),
                    .gtqi => try self.readOnlyRegister3(line_reader, o),
                    .ltqi => try self.readOnlyRegister3(line_reader, o),

                    .eqf =>  try self.readOnlyRegister3(line_reader, o),
                    .neqf => try self.readOnlyRegister3(line_reader, o),
                    .gtf =>  try self.readOnlyRegister3(line_reader, o),
                    .ltf =>  try self.readOnlyRegister3(line_reader, o),
                    .gtqf => try self.readOnlyRegister3(line_reader, o),
                    .ltqf => try self.readOnlyRegister3(line_reader, o),

                    .eqb =>  try self.readOnlyRegister3(line_reader, o),
                    .neqb => try self.readOnlyRegister3(line_reader, o),
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

    fn readRegister(reader: std.io.AnyReader, first_byte: ?u8) (anyerror || SyntaxError)!u8 {
        if ((first_byte == null and try reader.readByte() != '$') or (first_byte != null and first_byte.? != '$')) {
            std.debug.print("First byte (readRegister): {?c}\n", .{first_byte});
            return error.Syntax;
        }

        var number: u8 = 0;

        while (true) {
            const byte = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => return number,
                else => return err,
            };
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
    }

    fn readDigit(byte: u8) SyntaxError!u8 {
        if (byte >= '0' and byte <= '9') {
            return byte - '0';
        }
    }

    fn readIntegerIteration(number: *u16, first: *bool, sign: *bool, byte: u8) (SyntaxError || error { Done })!void {
        number.* *= 10;

        if (first.* and byte == '-') {
            sign.* = true;
        } else if (byte >= '0' and byte <= '9') {
            number.* += byte - '0';
        } else if (std.ascii.isWhitespace(byte)) {
            return error.Done;
        } else {
            return error.Syntax;
        }

        first.* = false;
    }

    fn makeSigned(number: u16, sign: bool) i16 {
        const signed = @as(i16, @intCast(number));

        if (sign) {
            return -signed;
        } else {
            return signed;
        }
    }

    fn readInteger(reader: std.io.AnyReader, first_byte: ?u8) (anyerror || SyntaxError)!i16 {
        var number: u16 = 0;
        var first = true;
        var sign = false;

        if (first_byte) |byte| {
            readIntegerIteration(&number, &first, &sign, byte) catch |err| switch (err) {
                error.Done => return error.Syntax,
                else => return err,
            };
        }

        while (true) {
            const byte = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => return if (first) error.Syntax else makeSigned(number, sign),
                else => return err,
            };

            readIntegerIteration(&number, &first, &sign, byte) catch |err| switch (err) {
                error.Done => return makeSigned(number, sign),
                else => return err,
            };
        }

        return number;
    }

    fn readNameIteration(list: *std.ArrayList(u8), first: *bool, byte: u8) (std.mem.Allocator.Error || SyntaxError || error { Done })!void {
        if (byte >= 'a' and byte <= 'z' or byte >= 'A' and byte <= 'Z' or byte == '_') {
            try list.append(byte);
        } else if (first and byte >= '0' and byte <= '9') {
            try list.append(byte);
        } else if (std.ascii.isWhitespace(byte)) {
            return error.Done;
        } else {
            return error.Syntax;
        }

        first.* = false;
    }

    /// Caller owns returned memory.
    fn readName(self: *Assembler, reader: std.io.AnyReader, first_byte: ?u8) (std.mem.Allocator.Error || anyerror || SyntaxError)![:0]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        errdefer list.deinit();

        var first = true;

        if (first_byte) |byte| {
            readNameIteration(&list, &first, byte) catch |err| switch (err) {
                error.Done => return error.Syntax,
                else => return err,
            };
        }

        while (reader.readByte()) |byte| {
            readNameIteration(&list, &first, byte) catch |err| switch (err) {
                error.Done => return try list.toOwnedSliceSentinel(0),
                else => return err,
            };
        }

        return try list.toOwnedSliceSentinel(0);
    }

    fn skipWhitespaceAndReadByte(reader: std.io.AnyReader) anyerror!u8 {
        while (true) {
            const byte = try reader.readByte();

            if (!std.ascii.isWhitespace(byte)) {
                return byte;
            }
        }
    }

    fn addInstruction(self: *Assembler, opcode: Opcode, byte_2: u8, byte_3: u8, byte_4: u8) std.mem.Allocator.Error!void {
        const slice = try self.instructions.addManyAsArray(self.allocator, 4);
        slice.* = .{@intFromEnum(opcode), byte_2, byte_3, byte_4, };
    }

    fn readDbg(self: *Assembler, reader: std.io.AnyReader) (anyerror || SyntaxError)!void {
        const byte = try skipWhitespaceAndReadByte(reader);
        const register = try readRegister(reader, byte);

        try self.addInstruction(Opcode.dbg, register, 0, 0);
    }

    fn readLoadInt(self: *Assembler, reader: std.io.AnyReader) (anyerror || SyntaxError)!void {
        var byte = try skipWhitespaceAndReadByte(reader);
        const register = try readRegister(reader, byte);

        byte = try skipWhitespaceAndReadByte(reader);
        const number = try readInteger(reader, byte);

        try self.addInstruction(Opcode.load_int, register, @bitCast(@as(u8, @intCast(number & 0xFF))), @bitCast(@as(u8, @intCast((@as(u16, @bitCast(number)) >> 8) & 0xFF))));
    }

    fn readOnlyRegister1(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        const byte = try skipWhitespaceAndReadByte(reader);
        const register = try readRegister(reader, byte);

        try self.addInstruction(opcode, register, 0, 0);
    }

    fn readOnlyRegister2(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        var byte = try skipWhitespaceAndReadByte(reader);
        const register1 = try readRegister(reader, byte);

        byte = try skipWhitespaceAndReadByte(reader);
        const register2 = try readRegister(reader, byte);

        try self.addInstruction(opcode, register1, register2, 0);
    }

    fn readOnlyRegister3(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        var byte = try skipWhitespaceAndReadByte(reader);
        const register1 = try readRegister(reader, byte);

        byte = try skipWhitespaceAndReadByte(reader);
        const register2 = try readRegister(reader, byte);

        byte = try skipWhitespaceAndReadByte(reader);
        const register3 = try readRegister(reader, byte);

        try self.addInstruction(opcode, register1, register2, register3);
    }
};
