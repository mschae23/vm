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

    OpcodeItem { .name = "jmpdy", .opcode = Opcode.jmpdy, }, // Has to be before .jmp, otherwise that one overrides .jmpdy
    OpcodeItem { .name = "jmp", .opcode = Opcode.jmp, },
    OpcodeItem { .name = "jro", .opcode = Opcode.jro, },
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
    labels: std.StringHashMap(u16),
    future_labels: std.StringHashMap(std.ArrayListUnmanaged(usize)),

    /// Input has to be ASCII text.
    pub fn init(allocator: std.mem.Allocator, reader: std.fs.File.Reader) std.mem.Allocator.Error!Assembler {
        return Assembler {
            .allocator = allocator,
            .reader = reader,
            .instructions = try std.ArrayListUnmanaged(u8).initCapacity(allocator, 0),
            .constants = try std.ArrayListUnmanaged(vm.Value).initCapacity(allocator, 0),
            .labels = std.StringHashMap(u16).init(allocator),
            .future_labels = std.StringHashMap(std.ArrayListUnmanaged(usize)).init(allocator),
        };
    }

    pub fn deinit(self: *Assembler) void {
        {
            var keys = self.labels.keyIterator();

            while (keys.next()) |key| {
                self.allocator.free(@as([:0]const u8, @ptrCast(key.*)));
            }

            self.labels.deinit();
        }

        {
            var keys = self.future_labels.keyIterator();

            while (keys.next()) |key| {
                self.allocator.free(@as([:0]const u8, @ptrCast(key.*)));
            }

            var values = self.future_labels.valueIterator();

            while (values.next()) |value| {
                value.deinit(self.allocator);
            }

            self.future_labels.deinit();
        }
    }

    /// Caller owns returned memory.
    pub fn assemble(self: *Assembler) !Assembly {
        errdefer self.instructions.deinit(self.allocator);
        errdefer self.constants.deinit(self.allocator);

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
            try self.assembleLine(line);

            fixedBufferStream.pos = 0;
        }
    }

    pub fn assembleLine(self: *Assembler, line: []const u8) !void {
        if (std.mem.startsWith(u8, line, "#")) {
            return;
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

        if (opcode) |o| {
            var fixedBufferStream = std.io.fixedBufferStream(rest);
            const line_reader = fixedBufferStream.reader().any();

            switch (o) {
                .nop => try self.addInstruction(o, 0, 0, 0),
                .halt => try self.addInstruction(o, 0, 0, 0),
                .igl => {
                    std.debug.print("Illegal instruction `IGL` must not be used.\n", .{});
                    return error.Illegal;
                },

                .dbg => try self.readDbg(line_reader),
                .load_int => try self.readLoadInt(line_reader, Opcode.load_int),
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

                .jmp => try self.readJmp(line_reader, o),
                .jro => try self.readOnlyRegister1(line_reader, o),
                .jmpdy => try self.readOnlyRegister1(line_reader, o),
                .jez => try self.readConditionalJump(line_reader, o),
                .jnz => try self.readConditionalJump(line_reader, o),

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
        } else if (line.len == 0) {
            // Continue to next line
        } else {
            var fixedBufferStream = std.io.fixedBufferStream(line);
            const line_reader = fixedBufferStream.reader().any();

            const name = try self.readName(line_reader, true, null);
            const entry = try self.labels.getOrPut(name);

            const index: u16 = @intCast(self.instructions.items.len / 4);

            if (entry.found_existing) {
                printError("Label already used: {s}\n", .{name});
                return error.Syntax;
            } else {
                entry.value_ptr.* = index;
            }

            const future_entry = self.future_labels.fetchRemove(name);

            if (future_entry) |kv| {
                self.allocator.free(@as([:0]const u8, @ptrCast(kv.key)));

                for (kv.value.items) |i| {
                    self.instructions.items[i] =     @bitCast(@as(u8, @intCast(index & 0xFF)));
                    self.instructions.items[i + 1] = @bitCast(@as(u8, @intCast((index >> 8) & 0xFF)));
                }

                var list = kv.value;
                list.deinit(self.allocator);
            }
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
            printError("Registers must begin with '$': {?c}\n", .{first_byte});
            return error.Syntax;
        }

        var number: u8 = 0;

        while (true) {
            const byte = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => return number,
                else => return err,
            };

            if (byte >= '0' and byte <= '9') {
                number <<= 4;
                number += byte - '0';
            } else if (byte >= 'a' and byte <= 'f') {
                number <<= 4;
                number += byte - 'a' + 10;
            } else if (byte >= 'A' and byte <= 'F') {
                number <<= 4;
                number += byte - 'A' + 10;
            } else if (std.ascii.isWhitespace(byte)) {
                return number;
            } else {
                printError("Invalid character in hexadecimal register number: {c}\n", .{number});
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
        if (first.* and byte == '-') {
            sign.* = true;
        } else if (byte >= '0' and byte <= '9') {
            number.* *= 10;
            number.* += byte - '0';
        } else if (std.ascii.isWhitespace(byte)) {
            return error.Done;
        } else {
            printError("Invalid character in decimal integer number: {c}\n", .{byte});
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
                error.Done => {
                    printError("Integer number is empty\n", .{});
                    return error.Syntax;
                },
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

    fn readNameIteration(label: bool, list: *std.ArrayList(u8), first: *bool, byte: u8) (std.mem.Allocator.Error || SyntaxError || error { Done })!void {
        if (byte >= 'a' and byte <= 'z' or byte >= 'A' and byte <= 'Z' or byte == '_') {
            try list.append(byte);
        } else if (first.* and byte >= '0' and byte <= '9') {
            try list.append(byte);
        } else if (label and !first.* and byte == ':') {
            return error.Done;
        } else if (!label and !first.* and std.ascii.isWhitespace(byte)) {
            return error.Done;
        } else {
            printError("Invalid character in label name: {c}\n", .{byte});
            return error.Syntax;
        }

        first.* = false;
    }

    /// Caller owns returned memory.
    fn readName(self: *Assembler, reader: std.io.AnyReader, label: bool, first_byte: ?u8) (std.mem.Allocator.Error || anyerror || SyntaxError)![:0]u8 {
        var list = std.ArrayList(u8).init(self.allocator);
        errdefer list.deinit();

        var first = true;

        if (first_byte) |byte| {
            readNameIteration(label, &list, &first, byte) catch |err| switch (err) {
                error.Done => {
                    printError("Empty label name\n", .{});
                    return error.Syntax;
                },
                else => return err,
            };
        }

        while (true) {
            const byte = reader.readByte() catch |err| switch (err) {
                error.EndOfStream => return try list.toOwnedSliceSentinel(0),
                else => return err,
            };

            readNameIteration(label, &list, &first, byte) catch |err| switch (err) {
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

    fn readLoadInt(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        var byte = try skipWhitespaceAndReadByte(reader);
        const register = try readRegister(reader, byte);

        byte = try skipWhitespaceAndReadByte(reader);

        if (byte == ':') {
            const label = try self.readName(reader, false, null);
            const target = self.labels.get(label);

            if (target) |t| {
                self.allocator.free(label);
                try self.addInstruction(opcode, @bitCast(@as(u8, @intCast(t & 0xFF))), @bitCast(@as(u8, @intCast((t >> 8) & 0xFF))), 0);
            } else {
                errdefer self.allocator.free(label);

                try self.addInstruction(opcode, register, 0xFF, 0xFF);
                const entry = try self.future_labels.getOrPut(label);

                if (!entry.found_existing) {
                    entry.value_ptr.* = try std.ArrayListUnmanaged(usize).initCapacity(self.allocator, 1);
                }

                try entry.value_ptr.append(self.allocator, self.instructions.items.len - 2);

                if (entry.found_existing) {
                    self.allocator.free(label);
                }
            }
        } else {
            const number = try readInteger(reader, byte);
            return self.addInstruction(opcode, register, @bitCast(@as(u8, @intCast(number & 0xFF))), @bitCast(@as(u8, @intCast((@as(u16, @bitCast(number)) >> 8) & 0xFF))));
        }
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

    fn readJmp(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        const byte = try skipWhitespaceAndReadByte(reader);
        const label = try self.readName(reader, false, byte);
        const target = self.labels.get(label);

        if (target) |t| {
            self.allocator.free(label);
            try self.addInstruction(opcode, @bitCast(@as(u8, @intCast(t & 0xFF))), @bitCast(@as(u8, @intCast((t >> 8) & 0xFF))), 0);
        } else {
            errdefer self.allocator.free(label);

            try self.addInstruction(opcode, 0xFF, 0xFF, 0);
            const entry = try self.future_labels.getOrPut(label);

            if (!entry.found_existing) {
                entry.value_ptr.* = try std.ArrayListUnmanaged(usize).initCapacity(self.allocator, 1);
            }

            try entry.value_ptr.append(self.allocator, self.instructions.items.len - 3);

            if (entry.found_existing) {
                self.allocator.free(label);
            }
        }
    }

    fn readJro(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        const byte = try skipWhitespaceAndReadByte(reader);
        const register = try readRegister(reader, byte);

        try self.addInstruction(opcode, register, 0, 0);
    }

    fn readConditionalJump(self: *Assembler, reader: std.io.AnyReader, opcode: Opcode) (anyerror || SyntaxError)!void {
        var byte = try skipWhitespaceAndReadByte(reader);
        const register = try readRegister(reader, byte);

        byte = try skipWhitespaceAndReadByte(reader);
        const label = try self.readName(reader, false, byte);
        const target = self.labels.get(label);

        if (target) |t| {
            self.allocator.free(label);
            try self.addInstruction(opcode, register, @bitCast(@as(u8, @intCast(t & 0xFF))), @bitCast(@as(u8, @intCast((t >> 8) & 0xFF))));
        } else {
            errdefer self.allocator.free(label);

            try self.addInstruction(opcode, register, 0xFF, 0xFF);
            const entry = try self.future_labels.getOrPut(label);

            if (!entry.found_existing) {
                entry.value_ptr.* = try std.ArrayListUnmanaged(usize).initCapacity(self.allocator, 1);
            }

            try entry.value_ptr.append(self.allocator, self.instructions.items.len - 2);

            if (entry.found_existing) {
                self.allocator.free(label);
            }
        }
    }

    fn printError(comptime fmt: []const u8, args: anytype) void {
        return std.debug.print("[error | assembler] " ++ fmt, args);
    }
};
