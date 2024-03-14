const std = @import("std");

pub const Opcode = @import("./opcode.zig").Opcode;

pub const Instruction = extern struct {
    opcode: Opcode,
    operand1: u8,
    operand2: u8,
    operand3: u8,

    pub fn init(opcode: Opcode, operand1: u8, operand2: u8, operand3: u8) Instruction {
        return Instruction {
            .opcode = opcode,
            .operand1 = operand1, .operand2 = operand2, .operand3 = operand3,
        };
    }
};

pub const Value = union {
    int: i32,
    float: i64,
    boolean: bool,
};

test "Instruction size" {
    try std.testing.expectEqual(4, @sizeOf(Instruction));
}

pub const REGISTER_COUNT: usize = 32;

pub const VirtualMachine = struct {
    instructions: []const u8,
    ip: usize,

    constants: []const Value,

    registers: [REGISTER_COUNT]u64,

    pub fn init(instructions: []const u8, constants: []const Value) VirtualMachine {
        return VirtualMachine {
            .instructions = instructions,
            .ip = 0,
            .constants = constants,
            .registers = .{0} ** REGISTER_COUNT,
        };
    }

    pub fn run(self: *VirtualMachine) void {
        while (true) {
            if (self.ip >= self.instructions.len) {
                break;
            }

            switch (self.decodeOpcode()) {
                .nop => {
                    self.ip += 3;
                },
                .halt => {
                    printDebug("Halt.\n", .{});
                    break;
                },
                .igl => {
                    printDebug("Encountered illegal instruction at {}\n", .{self.ip});
                    break;
                },

                .dbg => {
                    const register = self.nextByte();
                    self.ip += 2;

                    const value = self.registers[register];
                    printDebugImportant("Register ${x} as int: {}, float: {d:.16}, bool: {}\n", .{register, @as(i64, @bitCast(value)), @as(f64, @bitCast(value)), 0 != value});
                },
                .load_int => {
                    const register = self.nextByte();
                    self.registers[register] = @as(u64, self.next2BytesLittle());
                },
                .load_true => {
                    const register = self.nextByte();
                    self.ip += 2;
                    self.registers[register] = 1;
                },
                .load_false => {
                    const register = self.nextByte();
                    self.ip += 2;
                    self.registers[register] = 0;
                },
                .load_const => {
                    const register = self.nextByte();
                    self.registers[register] = @as(*const u64, @ptrCast(&self.constants[self.next2BytesLittle()])).*;
                    self.ip += 2;
                },

                .mov => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = self.registers[from];
                },
                .convitof => {
                    const from = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(@as(f64, @floatFromInt(from)));
                },
                .convitob => {
                    const from = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @intFromBool(0 != from);
                },
                .convftoi => {
                    const from = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(@as(i64, @intFromFloat(from)));
                },
                .convbtoi => {
                    const from = 0 != self.registers[self.nextByte()];
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @as(u64, @intFromBool(from));
                },
                .clear => {
                    self.ip += 3;
                },

                .addi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a + b);
                },
                .subi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a - b);
                },
                .muli => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a * b);
                },
                .divi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@divTrunc(a, b));
                },
                .modi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@rem(a, b));
                },
                .negi => {
                    const from = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(-from);
                },

                .addf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a + b);
                },
                .subf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a - b);
                },
                .mulf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a * b);
                },
                .divf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a / b);
                },
                .modf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@rem(a, b));
                },
                .negf => {
                    const from = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(-from);
                },

                .addb => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = 0 != self.registers[self.nextByte()];
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(a + @intFromBool(b));
                },
                .negb => {
                    const from = 0 != self.registers[self.nextByte()];
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @intFromBool(!from);
                },

                .jmp => {
                    const target = self.next2BytesLittle();
                    self.ip = target * 4;
                },
                .jro => {
                    const offset: isize = @intCast(@as(i64, @bitCast(self.registers[self.nextByte()])));
                    self.ip = @as(usize, @intCast(@as(isize, @intCast(self.ip - 3)) + (offset * 4)));
                },
                .jmpdy => {
                    const target: usize = @intCast(@as(i64, @bitCast(self.registers[self.nextByte()])));
                    self.ip = target * 4;
                },
                .jez => {
                    const register = self.nextByte();
                    const target = self.next2BytesLittle();

                    if (0 == self.registers[register]) {
                        self.ip = target * 4;
                    }
                },
                .jnz => {
                    const register = self.nextByte();
                    const target = self.next2BytesLittle();

                    if (0 != self.registers[register]) {
                        self.ip = target * 4;
                    }
                },

                .eqi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a == b));
                },
                .neqi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a != b));
                },
                .gti => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a > b));
                },
                .lti => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a < b));
                },
                .gtqi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a >= b));
                },
                .ltqi => {
                    const a = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(i64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a <= b));
                },

                .eqf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a == b));
                },
                .neqf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a != b));
                },
                .gtf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a > b));
                },
                .ltf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a < b));
                },
                .gtqf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a >= b));
                },
                .ltqf => {
                    const a = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const b = @as(f64, @bitCast(self.registers[self.nextByte()]));
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(a <= b));
                },

                .eqb => {
                    const a = 0 != self.registers[self.nextByte()];
                    const b = 0 != self.registers[self.nextByte()];
                    const target = self.nextByte();
                    self.registers[target] = @intFromBool(a == b);
                },
                .neqb => {
                    const a = 0 != self.registers[self.nextByte()];
                    const b = 0 != self.registers[self.nextByte()];
                    const target = self.nextByte();
                    self.registers[target] = @intFromBool(a != b);
                },
            }
        }
    }

    fn decodeOpcode(self: *VirtualMachine) Opcode {
        const opcode: Opcode = @enumFromInt(self.instructions[self.ip]);

        printDebug("ip: {: >4} (inst: {: >4}) 0x{x:0>2} 0x{x:0>2} 0x{x:0>2}: {s} ({})\n", .{self.ip, self.ip / 4,
            self.instructions[self.ip + 1], self.instructions[self.ip + 2], self.instructions[self.ip + 3],
            @tagName(opcode), @intFromEnum(opcode)});

        self.ip += 1;
        return opcode;
    }

    fn nextByte(self: *VirtualMachine) u8 {
        const result = self.instructions[self.ip];
        self.ip += 1;
        return result;
    }

    fn next2BytesLittle(self: *VirtualMachine) u16 {
        const result = std.mem.littleToNative(u16, std.mem.bytesToValue(u16, &.{self.instructions[self.ip], self.instructions[self.ip + 1]}));
        self.ip += 2;
        return result;
    }

    fn next2BytesSignedLittle(self: *VirtualMachine) i16 {
        const result = std.mem.littleToNative(i16, std.mem.bytesToValue(i16, &.{self.instructions[self.ip], self.instructions[self.ip + 1]}));
        self.ip += 2;
        return result;
    }

    fn printDebug(comptime fmt: []const u8, args: anytype) void {
        return std.debug.print("[debug | vm] " ++ fmt, args);
    }

    fn printDebugImportant(comptime fmt: []const u8, args: anytype) void {
        return std.debug.print("[debug | vm] " ++ fmt, args);
    }
};
