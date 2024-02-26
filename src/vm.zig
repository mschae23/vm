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
                    print("Halt.\n", .{});
                    break;
                },
                .igl => {
                    print("Encountered illegal instruction at {}\n", .{self.ip});
                    break;
                },

                .load_nil => {
                    print("Encountered illegal instruction (LOAD NIL) at {}\n", .{self.ip});
                    break;
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
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(@as(f64, @floatFromInt(@as(i64, @bitCast(self.registers[from])))));
                },
                .convitob => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @intFromBool(0 != @as(i64, @bitCast(self.registers[from])));
                },
                .convftoi => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(@as(i64, @intFromFloat(@as(f64, @bitCast(self.registers[from])))));
                },
                .convbtoi => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @as(u64, @intFromBool(0 != self.registers[from]));
                },
                .clear => {
                    self.ip += 3;
                },

                .addi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(i64, @bitCast(self.registers[a])) + @as(i64, @bitCast(self.registers[b])));
                },
                .subi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(i64, @bitCast(self.registers[a])) - @as(i64, @bitCast(self.registers[b])));
                },
                .muli => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(i64, @bitCast(self.registers[a])) * @as(i64, @bitCast(self.registers[b])));
                },
                .divi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@divTrunc(@as(i64, @bitCast(self.registers[a])), @as(i64, @bitCast(self.registers[b]))));
                },
                .modi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@rem(@as(i64, @bitCast(self.registers[a])), @as(i64, @bitCast(self.registers[b]))));
                },
                .negi => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(-@as(i64, @bitCast(self.registers[from])));
                },

                .addf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(f64, @bitCast(self.registers[a])) + @as(f64, @bitCast(self.registers[b])));
                },
                .subf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(f64, @bitCast(self.registers[a])) - @as(f64, @bitCast(self.registers[b])));
                },
                .mulf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(f64, @bitCast(self.registers[a])) * @as(f64, @bitCast(self.registers[b])));
                },
                .divf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(f64, @bitCast(self.registers[a])) / @as(f64, @bitCast(self.registers[b])));
                },
                .modf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@rem(@as(f64, @bitCast(self.registers[a])), @as(f64, @bitCast(self.registers[b]))));
                },
                .negf => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @bitCast(-@as(f64, @bitCast(self.registers[from])));
                },

                .addb => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @bitCast(@as(i64, @bitCast(self.registers[a])) + @intFromBool(0 != self.registers[b]));
                },
                .negb => {
                    const from = self.nextByte();
                    const to = self.nextByte();
                    self.ip += 1;
                    self.registers[to] = @intFromBool(!(0 != self.registers[from]));
                },

                .jmp => {
                    const target = self.next2BytesLittle();
                    self.ip = target * 4;
                },
                .jmpf => {
                    const offset = self.next2BytesSignedLittle();
                    self.ip = @as(usize, @intCast(@as(isize, @intCast(self.ip - 3)) + (offset * 4)));
                },
                .jmpb => {
                    const offset = self.next2BytesSignedLittle();
                    self.ip = @as(usize, @intCast(@as(isize, @intCast(self.ip - 3)) - (offset * 4)));
                },
                .jez => {
                    const register = self.nextByte();
                    const offset = self.next2BytesSignedLittle();

                    if (0 == self.registers[register]) {
                        self.ip = @as(usize, @intCast(@as(isize, @intCast(self.ip - 4)) + (offset * 4)));
                    }
                },
                .jnz => {
                    const register = self.nextByte();
                    const offset = self.next2BytesSignedLittle();

                    if (0 != self.registers[register]) {
                        self.ip = @as(usize, @intCast(@as(isize, @intCast(self.ip - 4)) + (offset * 4)));
                    }
                },

                .eqi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(i64, @bitCast(self.registers[a])) == @as(i64, @bitCast(self.registers[b]))));
                },
                .neqi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(i64, @bitCast(self.registers[a])) != @as(i64, @bitCast(self.registers[b]))));
                },
                .gti => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(i64, @bitCast(self.registers[a])) > @as(i64, @bitCast(self.registers[b]))));
                },
                .lti => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(i64, @bitCast(self.registers[a])) < @as(i64, @bitCast(self.registers[b]))));
                },
                .gtqi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(i64, @bitCast(self.registers[a])) >= @as(i64, @bitCast(self.registers[b]))));
                },
                .ltqi => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(i64, @bitCast(self.registers[a])) <= @as(i64, @bitCast(self.registers[b]))));
                },

                .eqf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(f64, @bitCast(self.registers[a])) == @as(f64, @bitCast(self.registers[b]))));
                },
                .neqf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(f64, @bitCast(self.registers[a])) != @as(f64, @bitCast(self.registers[b]))));
                },
                .gtf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(f64, @bitCast(self.registers[a])) > @as(f64, @bitCast(self.registers[b]))));
                },
                .ltf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(f64, @bitCast(self.registers[a])) < @as(f64, @bitCast(self.registers[b]))));
                },
                .gtqf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(f64, @bitCast(self.registers[a])) >= @as(f64, @bitCast(self.registers[b]))));
                },
                .ltqf => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @as(u64, @intFromBool(@as(f64, @bitCast(self.registers[a])) <= @as(f64, @bitCast(self.registers[b]))));
                },

                .eqb => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @intFromBool((0 != self.registers[a]) == (0 != self.registers[b]));
                },
                .neqb => {
                    const a = self.nextByte();
                    const b = self.nextByte();
                    const target = self.nextByte();
                    self.registers[target] = @intFromBool((0 != self.registers[a]) != (0 != self.registers[b]));
                },
            }
        }
    }

    fn decodeOpcode(self: *VirtualMachine) Opcode {
        const opcode: Opcode = @enumFromInt(self.instructions[self.ip]);

        print("ip: {}; inst: {}; op: {s} ({})\n", .{self.ip, self.ip / 4, @tagName(opcode), @intFromEnum(opcode)});

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

    fn print(comptime fmt: []const u8, args: anytype) void {
        return std.debug.print("[debug | vm ] " ++ fmt, args);
    }
};
