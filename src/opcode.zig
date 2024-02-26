pub const Opcode = enum(u8) {
    nop = 0,  // No-op
    halt = 1, // Halt
    igl = 2,  // Illegal

    load_nil = 3,   // Load nil
    load_int = 4,   // Load int
    load_true = 5,  // Load true
    load_false = 6, // Load false
    load_const = 7, // Load from constant table

    mov = 8,       // Move between registers
    convitof = 9,  // Convert i64 to f64
    convitob = 10, // Convert i64 to bool
    convftoi = 11, // Convert f64 to i64
    convbtoi = 12, // Convert bool to i64
    clear = 13,    // Clear register

    addi = 14, // Add two i64 registers
    subi = 15, // Subtract two i64 registers
    muli = 16, // Multiply two i64 registers
    divi = 17, // Divide two i64 registers
    modi = 18, // Modulo two i64 registers
    negi = 19, // Negate an i64 register
    addf = 20, // Add two f64 registers
    subf = 21, // Subtract two f64 registers
    mulf = 22, // Multiply two f64 registers
    divf = 23, // Divide two f64 registers
    modf = 24, // Modulo two f64 registers
    negf = 25, // Negate an f64 register
    addb = 26, // Add a bool register to an i64 register
    negb = 27, // Negate a bool register

    jmp = 28,  // Jump to instruction (set IP)
    jmpf = 29, // Jump forwards (increment IP)
    jmpb = 30, // Jump backwards (decrement IP)
    jez = 31,  // Jump if equal zero (jump if bool register == `false`)
    jnz = 32,  // Jump if not equal zero (jump if bool register == `true`)

    eqi = 33,  // Test for i64 equality
    neqi = 34, // Test for i64 inequality
    gti = 35,  // Test greater than for i64
    lti = 36,  // Test less than for i64
    gtqi = 37, // Test greater than or equal for i64
    ltqi = 38, // Test less than or equal for i64
    eqf = 39,  // Test for f64 equality
    neqf = 40, // Test for f64 inequality
    gtf = 41,  // Test greater than for f64
    ltf = 42,  // Test less than for f64
    gtqf = 43, // Test greater than or equal for f64
    ltqf = 44, // Test less than or equal for f64
    eqb = 45,  // Test for bool equality
    neqb = 46, // Tets for bool inequality
};
