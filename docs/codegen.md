# Ciel Code Generation Documentation

## Overview

The Ciel code generation module translates Three-Address Code (TAC) intermediate representation into RISC-V 64-bit assembly code. The module follows a multi-stage pipeline architecture:

1. **Instruction Selection** - Lower TAC to machine instructions with virtual registers
2. **Control Flow Graph Construction** - Build basic blocks and CFG edges
3. **Register Allocation** - Assign physical registers using linear scan allocation
4. **Peephole Optimization** - Apply local optimizations on instruction sequences
5. **Assembly Emission** - Generate final RISC-V assembly code

## Architecture

### Module Structure

```
codegen/
├── riscv64_codegen.hpp/cpp       - Main entry point and public API
├── riscv64_backend.hpp/cpp       - Backend orchestration and emission
├── riscv64_instruction.hpp/cpp   - Machine instruction representation
├── riscv64_register.hpp          - Register definitions and utilities
├── riscv64_frame.hpp/cpp         - Stack frame layout management
├── riscv64_regalloc.hpp/cpp      - Linear scan register allocator
└── riscv64_peephole.hpp/cpp      - Peephole optimizer
```

## Key Components

### 1. RiscV64Codegen (Entry Point)

**File**: `riscv64_codegen.hpp`

The main interface for code generation. Uses the PIMPL idiom to hide implementation details.

```cpp
class RiscV64Codegen {
public:
    RiscV64Codegen(const TACProgram &program,
                   SymbolTable &symtab,
                   TypeFactory &types);

    void emit(std::ostream &os);           // Emit to stream
    void emit_to_file(const std::string &path);  // Emit to file
};
```

**Usage Example**:
```cpp
RiscV64Codegen codegen(tac_program, symbol_table, type_factory);
codegen.emit_to_file("output.s");
```

---

### 2. RiscV64Backend (Orchestrator)

**File**: `riscv64_backend.hpp`

Orchestrates the entire code generation pipeline and manages assembly emission.

#### Key Classes

##### RiscV64Backend
Main backend class that coordinates all code generation phases.

**Responsibilities**:
- Lower TAC functions to machine functions
- Manage global data and string literals
- Emit assembly sections (.text, .data, .bss, .rodata)
- Handle libc function aliases
- Manage float constants and jump tables

**Key Methods**:
```cpp
void emit(std::ostream &os);                    // Generate complete assembly
std::string add_float_constant(double value);   // Register float literal
std::string add_jump_table(const vector<string> &labels);  // Create jump table
```

**Assembly Sections Emitted**:
- `.option pic` - Position-independent code directive
- `.rodata` - String literals and float constants
- `.bss` - Uninitialized globals
- `.data` - Initialized globals
- `.init_array` - Global constructors
- `.fini_array` - Global destructors
- `.text` - Function code

##### MachineFunction
Represents a single function after instruction selection.

**Components**:
- Name (mangled)
- Machine instructions (with virtual registers)
- Basic blocks (CFG nodes)
- Frame layout (stack management)
- Virtual register counter

**Key Methods**:
```cpp
void add_instruction(MachineInstr instr);
VirtReg get_next_vreg();
void build_cfg();  // Build control flow graph
```

##### InstructionSelector
Lowers TAC instructions to machine instructions with virtual registers.

**Key Responsibilities**:
- Map TAC temporaries to virtual registers
- Handle function calling conventions (RISC-V ABI)
- Manage parameter passing (registers a0-a7, fa0-fa7, stack)
- Handle aggregate types (pass-by-pointer)
- Generate loads/stores with proper addressing modes
- Track float vs integer register classes

**Calling Convention**:
- Integer arguments: `a0-a7` (8 registers)
- Float arguments: `fa0-fa7` (8 registers)
- Additional arguments: passed on stack
- Return value: `a0-a1` (integers), `fa0-fa1` (floats)
- Aggregate returns: implicit pointer in `a0` (sret convention)

**TAC Instruction Lowering**:
- `ENTER` → Prologue setup, parameter copying
- `RETURN` → Copy return value to a0/fa0, epilogue
- `ASSIGN` → Move or load immediate
- `BINARY_OP` → Arithmetic/logical instructions
- `CALL` → Parameter setup, call instruction, result retrieval
- `LOAD/STORE` → Memory operations with offset calculation
- `CAST` → Type conversion instructions
- etc.

**Important Constants**:
```cpp
constexpr size_t MAX_INT_REGS = 8;      // a0-a7
constexpr size_t MAX_FLOAT_REGS = 8;    // fa0-fa7
constexpr size_t POINTER_SIZE = 8;      // RV64
constexpr size_t WORD_SIZE = 8;         // RV64
constexpr int32_t MIN_IMM12 = -2048;    // RISC-V I-type immediate range
constexpr int32_t MAX_IMM12 = 2047;
```

---

### 3. Machine Instructions

**File**: `riscv64_instruction.hpp`

#### MachineOpcode Enum
Defines all supported RISC-V instructions including:
- **Pseudo-instructions**: `NOP`, `LI`, `LA`, `MV`, `J`, `RET`, `CALL`
- **Arithmetic**: `ADD`, `SUB`, `MUL`, `DIV`, `REM`, `ADDI`
- **Logical**: `AND`, `OR`, `XOR`, `ANDI`, `ORI`, `XORI`
- **Shifts**: `SLL`, `SRL`, `SRA`, `SLLI`, `SRLI`, `SRAI`
- **Comparisons**: `SLT`, `SLTU`, `SEQZ`, `SNEZ`
- **Branches**: `BEQ`, `BNE`, `BLT`, `BGE`, `BLTU`, `BGEU`
- **Loads**: `LB`, `LH`, `LW`, `LD`, `LBU`, `LHU`, `LWU`
- **Stores**: `SB`, `SH`, `SW`, `SD`
- **Float Loads/Stores**: `FLD`, `FSD`
- **Float Arithmetic**: `FADD_D`, `FSUB_D`, `FMUL_D`, `FDIV_D`, `FSQRT_D`
- **Float Comparisons**: `FEQ_D`, `FLT_D`, `FLE_D`
- **Float Conversions**: `FCVT_W_D`, `FCVT_D_W`, etc.
- **Float Move**: `FMV_D`, `FMV_X_D`, `FMV_D_X`

#### Operand Types
```cpp
struct ImmOperand { int64_t value; };           // Immediate constant
struct RegOperand { PhysReg reg; };             // Physical register
struct VRegOperand { VirtReg vreg; };           // Virtual register
struct LabelOperand { std::string label; };     // Symbol/label
struct MemOperand { PhysReg base; int64_t offset; };  // Memory [base+offset]
```

#### MachineInstr Class
Represents a single machine instruction.

**Fields**:
- `opcode` - Instruction operation
- `defs` - List of defined virtual registers
- `uses` - List of used virtual registers
- `operands` - Instruction operands (mixed types)
- `label` - Optional label (for LABEL pseudo-op)

**Builder Pattern**:
```cpp
MachineInstr instr = MachineInstr(MachineOpcode::ADD)
    .add_def(v1)
    .add_use(v2)
    .add_use(v3);
```

**Helper Functions**:
```cpp
// Common instruction builders
MachineInstr make_add(VirtReg dst, VirtReg src1, VirtReg src2);
MachineInstr make_addi(VirtReg dst, VirtReg src, int64_t imm);
MachineInstr make_li(VirtReg dst, int64_t imm);
MachineInstr make_la(VirtReg dst, const std::string &label);
MachineInstr make_mv(VirtReg dst, VirtReg src);
MachineInstr make_lw(VirtReg dst, PhysReg base, int64_t offset);
MachineInstr make_sw(VirtReg src, PhysReg base, int64_t offset);
MachineInstr make_label(const std::string &label);
MachineInstr make_call(const std::string &target);
MachineInstr make_ret();
```

#### MachineBasicBlock
Represents a basic block in the CFG.

**Fields**:
```cpp
size_t start_instr_idx;              // First instruction index
size_t end_instr_idx;                // Last instruction index
std::string label;                   // Block label (if any)
std::vector<size_t> successors;      // CFG successors
std::vector<size_t> predecessors;    // CFG predecessors
```

---

### 4. Register Management

**File**: `riscv64_register.hpp`

#### PhysReg Enum
Enumerates all RISC-V 64-bit physical registers (integer + floating-point).

**Integer Registers** (x0-x31):
- `ZERO` (x0) - Hardwired zero
- `RA` (x1) - Return address
- `SP` (x2) - Stack pointer
- `GP` (x3) - Global pointer (not used)
- `TP` (x4) - Thread pointer (not used)
- `T0-T6` (x5-x7, x28-x31) - Temporaries (caller-saved)
- `S0/FP` (x8) - Frame pointer (callee-saved)
- `S1-S11` (x9, x18-x27) - Saved registers (callee-saved)
- `A0-A7` (x10-x17) - Arguments/return values (caller-saved)

**Float Registers** (f0-f31):
- `FT0-FT11` (f0-f7, f28-f31) - Temporaries (caller-saved)
- `FS0-FS11` (f8-f9, f18-f27) - Saved (callee-saved)
- `FA0-FA7` (f10-f17) - Arguments/return values (caller-saved)

#### VirtReg Type
Unlimited virtual registers used during instruction selection.
```cpp
using VirtReg = uint32_t;
constexpr VirtReg INVALID_VREG = 0;
```

#### Register Classification

**Caller-Saved Registers** (no save/restore in callee):
- Temporaries: `T0-T6`, `FT0-FT11`
- Arguments: `A0-A7`, `FA0-FA7`
- Return address: `RA`

**Callee-Saved Registers** (must save/restore if used):
- Saved: `S0-S11`, `FS0-FS11`

**Allocatable Registers** (for register allocation):
- Integer: `T3-T6` (4), `A0-A7` (8), `S1-S11` (11) = **23 registers**
- Float: `FT3-FT11` (9), `FA0-FA7` (8), `FS0-FS11` (12) = **29 registers**

**Reserved/Special Purpose**:
- `ZERO`, `SP`, `GP`, `TP` - Never allocated
- `S0/FP` - Reserved as frame pointer
- `T0-T2`, `FT0-FT2` - Reserved for spill temporaries (6 total)

#### Utility Functions
```cpp
constexpr std::string_view get_reg_name(PhysReg reg);
constexpr bool is_caller_saved(PhysReg reg);
constexpr bool is_callee_saved(PhysReg reg);
constexpr bool is_allocatable(PhysReg reg);
constexpr bool is_fp_allocatable(PhysReg reg);

// Get allocatable register pools
std::array<PhysReg, 23> get_allocatable_regs();      // Integer
std::array<PhysReg, 29> get_fp_allocatable_regs();   // Float
std::array<PhysReg, 3> get_int_spill_temps();        // T0-T2
std::array<PhysReg, 3> get_fp_spill_temps();         // FT0-FT2
```

---

### 5. Stack Frame Layout

**File**: `riscv64_frame.hpp`

#### FrameLayout Class
Manages stack frame allocation following the RISC-V calling convention.

**Frame Layout** (high to low address):
```
[caller's frame]
[return address (RA)]      ← saved by caller (CALL instruction)
[saved FP (s0)]           ← FP points here
[saved callee regs]       ← s1-s11, fs0-fs11 (if used)
[local variables]
[spill slots]
[outgoing arguments > 8]
[padding for alignment]   ← SP points here (16-byte aligned)
```

**Constants**:
```cpp
constexpr int32_t FIXED_OVERHEAD = 16;        // RA (8) + FP (8)
constexpr int32_t MAX_CALLEE_SAVES = 96;      // 12 regs × 8 bytes
constexpr int32_t RESERVED_SPACE = 112;       // FIXED + MAX_CALLEE
```

**Key Methods**:
```cpp
// Allocate stack slot for local variable or temporary
int32_t allocate_slot(uint32_t size, uint32_t alignment = 4);

// Allocate/retrieve spill slot for virtual register
int32_t allocate_spill_slot(VirtReg vreg, uint32_t size);
StackSlot get_spill_slot(VirtReg vreg) const;

// Track callee-saved register usage
void mark_callee_reg_used(PhysReg reg);
const std::vector<PhysReg> &get_used_callee_regs() const;
int32_t get_callee_save_offset(PhysReg reg) const;

// Update frame requirements
void update_max_call_args(uint32_t num_args);
void update_max_parallel_move_spills(uint32_t num_spills);

// Finalize layout (compute total size, align to 16 bytes)
void finalize();

// Getters
int32_t get_frame_size() const;
int32_t get_ra_offset() const;
int32_t get_fp_offset() const;
```

**Stack Slot**:
```cpp
struct StackSlot {
    int32_t offset;  // Offset from FP (negative = below FP)
    uint32_t size;   // Size in bytes
};
```

---

### 6. Register Allocation

**File**: `riscv64_regalloc.hpp`

#### LinearScanAllocator
Implements linear scan register allocation with live interval analysis.

**Algorithm Overview**:
1. **Liveness Analysis** - Compute live-in/live-out sets for basic blocks
2. **Interval Construction** - Build live intervals for each virtual register
3. **Interval Splitting** - Split intervals at definition/use points (handles holes)
4. **Register Assignment** - Greedy allocation in order of interval start position
5. **Spilling** - Assign stack slots for registers that can't be allocated
6. **Call Site Handling** - Save/restore caller-saved registers across calls
7. **Instruction Rewriting** - Replace virtual registers with physical registers or memory references

**Register Classes**:
```cpp
enum class RegClass { INTEGER, FLOAT };
```

**Live Interval**:
```cpp
struct LiveInterval {
    VirtReg vreg;
    RegClass reg_class;
    size_t start;                    // First use position
    size_t end;                      // Last use position
    PhysReg assigned_reg;            // Allocated physical register
    int32_t spill_slot;              // Stack offset (if spilled)
    bool spilled;
    std::set<size_t> use_positions;  // All use points
    std::set<size_t> def_positions;  // All definition points
};
```

**Location Information**:
```cpp
struct LocationInfo {
    std::variant<PhysReg, Offset> value;  // Register or stack offset

    static LocationInfo in_register(PhysReg reg);
    static LocationInfo on_stack(Offset offset);

    bool is_register() const;
    bool is_stack() const;
    PhysReg get_register() const;
    Offset get_stack_offset() const;
};
```

**Key Methods**:
```cpp
void run();  // Main allocation entry point

// Query allocation results
std::optional<PhysReg> get_allocation(VirtReg vreg) const;
bool is_spilled(VirtReg vreg) const;
int32_t get_spill_slot(VirtReg vreg) const;
LocationInfo get_location(VirtReg vreg) const;

// Debug mode
static void set_debug_mode(bool debug);
```

**Allocation Strategy**:
- Process intervals in order of start position (greedy)
- Prefer caller-saved registers (no prologue/epilogue overhead)
- Use callee-saved registers when caller-saved exhausted
- Spill least-recently-used interval if all registers busy
- Separate pools for integer and float registers

**Spill Handling**:
- Spilled values stored on stack (frame layout manages slots)
- Load from stack before use, store after definition
- Uses reserved temporary registers (T0-T2, FT0-FT2) for loads/stores

---

### 7. Peephole Optimization

**File**: `riscv64_peephole.hpp`

#### PeepholeOptimizer
Applies local optimizations on instruction sequences after register allocation.

**Optimizations**:

1. **Redundant Move Elimination**
   - Remove `mv rd, rs` where `rd == rs`
   - Fold redundant register copies

2. **Redundant Load Elimination**
   - Remove `lw/ld rd, offset(rs)` followed immediately by identical load
   - Track recent memory loads

3. **Jump Optimization**
   - Remove unconditional jumps to next instruction
   - Simplify jump chains (`j L1; L1: j L2` → `j L2`)

4. **Arithmetic Simplification**
   - `addi rd, rs, 0` → `mv rd, rs` (or eliminate if rd == rs)
   - Strength reduction for multiplication by powers of 2
   - Constant folding where possible

**Usage**:
```cpp
PeepholeOptimizer optimizer(machine_function);
optimizer.run();  // Apply all optimizations
```

---

## Code Generation Pipeline

### Complete Flow

```
TAC Program
    ↓
┌─────────────────────────────────────┐
│ RiscV64Backend::lower_functions()  │
│ - For each TAC function:           │
│   1. Create MachineFunction        │
│   2. Run InstructionSelector       │
│   3. Build CFG                     │
│   4. Run LinearScanAllocator       │
│   5. Run PeepholeOptimizer         │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│ RiscV64Backend::emit()              │
│ - Emit preamble                    │
│ - Emit .rodata (strings, floats)   │
│ - Emit .bss/.data (globals)        │
│ - Emit .init_array/.fini_array     │
│ - Emit .text (functions)           │
│   - Prologue                       │
│   - Instructions                   │
│   - Epilogue                       │
└─────────────────────────────────────┘
    ↓
RISC-V Assembly (.s file)
```

### Function Compilation Stages

For each function:

1. **Instruction Selection**
   ```
   TAC: t1 = a + b
   ↓
   Machine: ADD v1, v2, v3  (virtual registers)
   ```

2. **CFG Construction**
   ```
   Build basic blocks and edges based on branches/labels
   ```

3. **Register Allocation**
   ```
   Machine: ADD v1, v2, v3
   ↓
   Machine: ADD a0, a1, a2  (physical registers)
   or
   Machine: LW t0, 16(fp)   (if spilled)
           ADD a0, t0, a2
           SW a0, 24(fp)
   ```

4. **Peephole Optimization**
   ```
   MV a0, a0  →  (removed)
   ADDI a0, a1, 0  →  MV a0, a1
   ```

5. **Emission**
   ```
   Generate assembly text with proper formatting
   ```

---

## Assembly Output Format

### Sections

#### Preamble
```asm
.option pic
.option norelax
```

#### Read-Only Data
```asm
.section .rodata
.align 3
.LC0:
    .string "Hello, World!"
.align 3
.FP0:
    .double 3.14159
```

#### Global Variables
```asm
.section .bss
.align 3
.globl _ZN4ciel6globalE
_ZN4ciel6globalE:
    .zero 8

.section .data
.align 3
.globl _ZN4ciel11initializedE
_ZN4ciel11initializedE:
    .quad 42
```

#### Function Code
```asm
.section .text
.align 2
.globl _ZN4ciel4mainEv
_ZN4ciel4mainEv:
    # Prologue
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32

    # Function body
    li a0, 42
    call _ZN4ciel5printEi

    # Epilogue
    li a0, 0
    ld ra, 24(sp)
    ld s0, 16(sp)
    addi sp, sp, 32
    ret
```

---

## RISC-V ABI Compliance

### Calling Convention

**Integer Arguments**: `a0-a7` (8 registers)
**Float Arguments**: `fa0-fa7` (8 registers)
**Excess Arguments**: Stack (above frame pointer)

**Return Values**:
- Integer/pointer: `a0` (or `a0-a1` for 128-bit)
- Float: `fa0` (or `fa0-fa1` for complex)
- Aggregate: Pointer in `a0` (caller allocates space)

**Stack Alignment**: 16 bytes at function call boundaries

### Register Preservation

**Caller-Saved** (not preserved across calls):
- `ra`, `t0-t6`, `a0-a7`, `ft0-ft11`, `fa0-fa7`

**Callee-Saved** (must be preserved):
- `sp`, `s0-s11`, `fs0-fs11`

### Function Prologue/Epilogue

**Prologue**:
```asm
addi sp, sp, -<frame_size>    # Allocate frame
sd ra, <ra_offset>(sp)         # Save return address
sd s0, <fp_offset>(sp)         # Save old frame pointer
addi s0, sp, <frame_size>      # Set new frame pointer
# Save used callee-saved registers
```

**Epilogue**:
```asm
# Restore callee-saved registers
ld ra, <ra_offset>(sp)
ld s0, <fp_offset>(sp)
addi sp, sp, <frame_size>      # Deallocate frame
ret
```

---

## Type System Integration

### Type Size Mapping

```cpp
bool    → 1 byte  (LB/SB)
i8      → 1 byte  (LB/SB)
i16     → 2 bytes (LH/SH)
i32     → 4 bytes (LW/SW)
i64     → 8 bytes (LD/SD)
f64     → 8 bytes (FLD/FSD)
ptr     → 8 bytes (LD/SD)
```

### Signedness

**Signed Types**: Use sign-extending loads (`LB`, `LH`, `LW`)
**Unsigned Types**: Use zero-extending loads (`LBU`, `LHU`, `LWU`)

### Aggregate Types

**Structs/Arrays**:
- Passed by pointer (copy pointer to `a0-a7`)
- Returned via implicit pointer in `a0` (sret convention)
- Member access: base pointer + offset calculation

---

## Error Handling

The codegen module assumes well-formed TAC input (validated by earlier passes). It will:
- Abort on internal consistency errors (assertions)
- Throw exceptions for file I/O errors
- Generate potentially invalid assembly for malformed input (garbage-in-garbage-out)

---

## Debugging Support

### Debug Mode

Enable verbose output in register allocator:
```cpp
LinearScanAllocator::set_debug_mode(true);
```

### Assembly Comments

Generated assembly includes comments for:
- Function names
- Basic block labels
- Frame offsets
- Saved registers

---

## Limitations and Future Work

### Current Limitations

1. **Target Architecture**: Only RISC-V 64-bit supported
2. **Optimization Level**: Basic peephole only (no global optimizations)
3. **Register Allocation**: Linear scan (not optimal, but fast)
4. **Floating Point**: Only double-precision (64-bit) supported
5. **SIMD**: No vector instruction support
6. **Position-Independent Code**: Always emits PIC code

### Future Enhancements

- [ ] Advanced optimizations (loop optimizations, inlining)
- [ ] Instruction scheduling
- [ ] Better spill code placement
- [ ] Debug information generation (DWARF)

---

## Usage Examples

### Basic Usage

```cpp
#include "codegen/riscv64_codegen.hpp"

// Assume we have TAC program, symbol table, type factory
TACProgram tac_program = /* ... */;
SymbolTable symbol_table = /* ... */;
TypeFactory type_factory = /* ... */;

// Create code generator
RiscV64Codegen codegen(tac_program, symbol_table, type_factory);

// Generate assembly to file
codegen.emit_to_file("output.s");

// Or to stream
std::ofstream out("output.s");
codegen.emit(out);
```

### Custom Assembly Processing

```cpp
#include "codegen/riscv64_backend.hpp"
#include <sstream>

// Generate to string stream
std::stringstream ss;
RiscV64Backend backend(tac_program, symbol_table, type_factory);
backend.emit(ss);

// Process assembly text
std::string assembly = ss.str();
// ... post-process ...
```

---

## References

- [RISC-V ISA Specification](https://riscv.org/technical/specifications/)
- [RISC-V Calling Convention](https://github.com/riscv-non-isa/riscv-elf-psabi-doc)
- [Linear Scan Register Allocation](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)
- [SSA Book - Code Generation](https://pfalcon.github.io/ssabook/latest/book-full.pdf)

---

## Glossary

- **TAC**: Three-Address Code - intermediate representation
- **Virtual Register (vreg)**: Temporary register during instruction selection (unlimited)
- **Physical Register (preg)**: Actual hardware register (limited)
- **Live Interval**: Range of instruction positions where a value is live
- **Spilling**: Storing a value to stack when no register available
- **Callee-Saved**: Register that must be preserved by called function
- **Caller-Saved**: Register that caller must save if needed across call
- **Frame Pointer (FP)**: Register pointing to current stack frame (s0/x8)
- **Stack Pointer (SP)**: Register pointing to top of stack (sp/x2)
- **ABI**: Application Binary Interface - calling convention rules
- **CFG**: Control Flow Graph - representation of program flow
- **Basic Block**: Sequence of instructions with single entry/exit
- **PIC**: Position-Independent Code - relocatable code
