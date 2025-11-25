# Ciel Three-Address Code (TAC) Documentation

## Overview

The Three-Address Code (TAC) module serves as Ciel's intermediate representation (IR) between the Abstract Syntax Tree (AST) and target-specific code generation. TAC is a linearized, low-level representation that simplifies code analysis and optimization while remaining architecture-independent.

**Key Characteristics**:
- Each instruction has at most three addresses (result, operand1, operand2)
- Explicit control flow with labels and jumps
- Explicit temporary variables (no nested expressions)
- Type information preserved for code generation
- Architecture-neutral (targets any backend)

## Module Structure

```
tac/
├── tac.hpp/cpp              - TAC IR data structures
├── tac_generator.hpp/cpp    - AST → TAC translation
└── tac_errors.hpp           - TAC generation error handling
```

---

## TAC Instruction Set

### TACOpcode Enumeration

#### Arithmetic Operations
```cpp
ADD     // result = operand1 + operand2
SUB     // result = operand1 - operand2
MUL     // result = operand1 * operand2
DIV     // result = operand1 / operand2
MOD     // result = operand1 % operand2
NEG     // result = -operand1
```

#### Bitwise Operations
```cpp
AND     // result = operand1 & operand2
OR      // result = operand1 | operand2
XOR     // result = operand1 ^ operand2
NOT     // result = ~operand1
SHL     // result = operand1 << operand2
SHR     // result = operand1 >> operand2
```

#### Logical Operations
```cpp
LAND    // result = operand1 && operand2 (logical AND)
LOR     // result = operand1 || operand2 (logical OR)
LNOT    // result = !operand1 (logical NOT)
```

#### Relational Operations
```cpp
EQ      // result = operand1 == operand2
NE      // result = operand1 != operand2
LT      // result = operand1 < operand2
LE      // result = operand1 <= operand2
GT      // result = operand1 > operand2
GE      // result = operand1 >= operand2
```

#### Assignment Operations
```cpp
ASSIGN  // result = operand1 (simple assignment)
COPY    // result = operand1 (copy operation)
```

#### Memory Operations
```cpp
LOAD        // result = *operand1 (load from address)
STORE       // *result = operand1 (store to address)
ADDR_OF     // result = &operand1 (address-of)
DEREF       // result = *operand1 (dereference)
```

#### Control Flow Operations
```cpp
LABEL       // Define a label (operand1 = label name)
GOTO        // Unconditional jump to label (operand1 = label)
IF_FALSE    // Jump to label if condition is false
IF_TRUE     // Jump to label if condition is true
```

#### Function Operations
```cpp
PARAM       // Push parameter for function call
CALL        // Call function (result = return value, operand1 = function)
RETURN      // Return from function (operand1 = return value, optional)
ENTER       // Function prologue (allocate frame)
LEAVE       // Function epilogue (deallocate frame)
```

#### Aggregate/Member Access Operations
```cpp
INDEX_ADDR      // result = &array[index] (array element address)
MEMBER_ADDR     // result = &struct.member (member address)
MEMBER_ACCESS   // Access struct/class member
LOAD_MEMBER     // result = base.member (load from member)
STORE_MEMBER    // base.member = value (store to member)
MEMBER_OFFSET   // result = &base + offset (calculate member offset)
```

#### Type Operations
```cpp
CAST    // result = (type)operand1 (type conversion)
```

#### Switch/Jump Table Operations
```cpp
JUMP_TABLE      // Jump using table: JUMP_TABLE index, table_label
JUMP_TABLE_INIT // Initialize jump table metadata
```

#### Special Operations
```cpp
NOP     // No operation
PHI     // SSA phi node (for future SSA support)
```

---

## Data Structures

### TACOperand

Represents an operand in a TAC instruction. Can be a symbol, temporary, constant, or label.

#### Operand Kinds
```cpp
enum class Kind {
    SYMBOL,      // Variable or function from symbol table
    TEMPORARY,   // Compiler-generated temporary (e.g., t1, t2)
    CONSTANT,    // Integer or floating-point literal
    LABEL,       // Jump target label
    NONE         // Invalid/uninitialized operand
};
```

#### Fields
```cpp
struct TACOperand {
    Kind kind;
    std::variant<SymbolPtr, std::string, int64_t, double> value;
    TypePtr type;

    // For member access operations
    std::string member_name;   // Name of accessed member
    size_t member_offset;      // Byte offset of member
    TypePtr base_type;         // Type of base object
};
```

#### Factory Methods
```cpp
static TACOperand symbol(SymbolPtr sym, TypePtr t = nullptr);
static TACOperand temporary(const std::string &name, TypePtr t);
static TACOperand constant_int(int64_t val, TypePtr t);
static TACOperand constant_float(double val, TypePtr t);
static TACOperand label(const std::string &name);
static TACOperand member_access(const TACOperand &base,
                                const std::string &member,
                                size_t offset,
                                TypePtr member_type);
```

#### Example Usage
```cpp
// Symbol operand
auto sym_op = TACOperand::symbol(my_symbol, int_type);

// Temporary operand
auto temp_op = TACOperand::temporary("t1", int_type);

// Integer constant
auto const_op = TACOperand::constant_int(42, int_type);

// Label operand
auto label_op = TACOperand::label("L1");
```

#### String Representation
```cpp
std::string to_string() const;
```

**Examples**:
- Symbol: `x`, `global_var`
- Temporary: `t1`, `t42`
- Member access: `obj.x`, `ptr.member`
- Constant: `42`, `3.14`
- Label: `L1`, `.str0`

---

### TACInstruction

Represents a single three-address code instruction.

#### Fields
```cpp
struct TACInstruction {
    TACOpcode opcode;        // Operation to perform
    TACOperand result;       // Destination operand
    TACOperand operand1;     // First source operand
    TACOperand operand2;     // Second source operand (optional)

    std::string comment;     // Human-readable comment

    // For jump table instructions
    std::vector<std::string> jump_table_labels;
    int64_t jump_table_min;
    int64_t jump_table_max;

    // For backpatching (forward jumps)
    size_t target_line_number;
    bool is_backpatch_target;
};
```

#### Constructor
```cpp
TACInstruction(TACOpcode op,
               TACOperand res = TACOperand(),
               TACOperand op1 = TACOperand(),
               TACOperand op2 = TACOperand());
```

#### String Representation
```cpp
std::string to_string(size_t line_offset = 0) const;
```

**Format Examples**:
```
0:  ENTER
1:  t1 = a ADD b
2:  t2 = t1 MUL c
3:  PARAM t2
4:  t3 = CALL print
5:  RETURN t3
```

---

### TACBasicBlock

Represents a basic block in the control flow graph (CFG).

#### Definition
A **basic block** is a maximal sequence of instructions with:
- Single entry point (first instruction)
- Single exit point (last instruction)
- No internal branches

#### Fields
```cpp
struct TACBasicBlock {
    std::string label;                              // Block label (if any)
    std::vector<TACInstructionPtr> instructions;    // Instructions in block
    std::vector<TACBasicBlockPtr> successors;       // CFG outgoing edges
    std::vector<TACBasicBlockPtr> predecessors;     // CFG incoming edges
};
```

#### Methods
```cpp
void add_instruction(TACInstructionPtr instr);
void add_successor(TACBasicBlockPtr succ);  // Also updates predecessor
```

#### Usage
Basic blocks are automatically created during function generation and are used for:
- Control flow analysis
- Dataflow analysis
- Optimization passes
- Register allocation (liveness analysis)

---

### TACFunction

Represents a complete function in TAC form.

#### Fields
```cpp
struct TACFunction {
    std::string name;                        // Original function name
    std::string mangled_name;                // Mangled name (for linking)
    TypePtr return_type;                     // Function return type
    std::vector<TACOperand> parameters;      // Function parameters
    std::vector<TACBasicBlockPtr> basic_blocks;  // CFG nodes
    TACBasicBlockPtr entry_block;            // Entry point
    TACBasicBlockPtr exit_block;             // Exit point (optional)

    // Generation state
    int temp_counter;                        // Next temporary number
    int label_counter;                       // Next label number
    ScopeID body_scope_id;                   // Function body scope
    SymbolTable &sym_table;                  // Symbol table reference

    // Backpatching support
    size_t current_instruction_number;       // Current position
    std::map<std::string, std::vector<TACInstructionPtr>> pending_jumps;
    std::map<std::string, size_t> emitted_labels;
};
```

#### Methods
```cpp
std::string new_temp(TypePtr type);              // Generate unique temporary
std::string new_label(const std::string &prefix = "L");  // Generate unique label
void add_block(TACBasicBlockPtr block);          // Add basic block
void print(size_t &line_number) const;           // Print TAC code
```

#### Temporary Naming
Temporaries are named using mangling to ensure uniqueness across scopes:
```cpp
std::string TACFunction::new_temp(TypePtr type) {
    std::string temp_name = "t" + std::to_string(temp_counter++);
    // Mangle temporary with scope information
    return mangle_temporary(temp_name, body_scope_id, sym_table);
}
```

**Example**: `_ZN4ciel4main1tE` (temporary `t` in function `ciel::main`)

#### Label Naming
```cpp
std::string TACFunction::new_label(const std::string &prefix) {
    return prefix + std::to_string(label_counter++);
}
```

**Example**: `L0`, `L1`, `LOOP_START2`

---

### TACProgram

Represents a complete translation unit in TAC form.

#### Fields
```cpp
struct TACProgram {
    std::vector<TACFunctionPtr> functions;           // All functions
    std::vector<SymbolPtr> global_variables;         // Global variables
    std::vector<StringLiteral> string_literals;      // String constants
    std::vector<GlobalInitializer> global_initializers;  // Init functions
};
```

#### Methods
```cpp
void add_function(TACFunctionPtr func);
void add_global(SymbolPtr sym);
void add_string_literal(const std::string &label, const std::string &value);
void add_global_initializer(SymbolPtr sym, bool needs_dtor = false);
void print() const;  // Print entire program
```

#### String Literals
```cpp
struct StringLiteral {
    std::string label;    // e.g., ".str0"
    std::string value;    // Actual string content
};
```

#### Global Initializers
```cpp
struct GlobalInitializer {
    SymbolPtr symbol;       // Global variable symbol
    bool needs_destructor;  // Requires cleanup function
};
```

---

## TACGenerator

Translates AST nodes into TAC instructions.

### Constructor
```cpp
TACGenerator();
```

Automatically accesses global `SymbolTable` and `TypeFactory` instances.

### Main API

#### Generate Complete Program
```cpp
Result<bool, std::vector<TACErrorInfo>>
generate(const std::vector<ASTNodePtr> &translation_unit);
```

Generates TAC for an entire translation unit (all top-level declarations).

**Returns**:
- `Ok(true)` on success
- `Err(errors)` with detailed error information

#### Access Results
```cpp
const TACProgram &get_program() const;
TACProgram &get_program();
const std::vector<TACErrorInfo> &get_errors() const;
```

---

### Expression Generation

#### Entry Point
```cpp
TACOperand generate_expression(ASTNodePtr node);
```

Recursively generates TAC for any expression, returning the operand holding the result.

#### Expression Handlers

##### Binary Operations
```cpp
TACOperand generate_binary_op(std::shared_ptr<BinaryExpr> expr);
```

**Example Translation**:
```cpp
// Source: a + b * c
// AST: BinaryExpr(+, a, BinaryExpr(*, b, c))

// Generated TAC:
t1 = b MUL c
t2 = a ADD t1
// Returns: TACOperand::temporary("t2", type)
```

**Short-Circuit Evaluation** (for `&&` and `||`):
```cpp
// Source: a && b
// TAC:
    t1 = a
    IF_FALSE t1 GOTO L_FALSE
    t2 = b
    IF_FALSE t2 GOTO L_FALSE
    t_result = 1
    GOTO L_END
L_FALSE:
    t_result = 0
L_END:
    // Returns: t_result
```

##### Unary Operations
```cpp
TACOperand generate_unary_op(std::shared_ptr<UnaryExpr> expr);
```

**Example**:
```cpp
// Source: -x
// TAC:
t1 = NEG x
// Returns: t1
```

**Address-of and Dereference**:
```cpp
// Source: &x
// TAC:
t1 = ADDR_OF x

// Source: *ptr
// TAC:
t1 = DEREF ptr
```

##### Assignment
```cpp
TACOperand generate_assignment(std::shared_ptr<AssignmentExpr> expr);
```

**Simple Assignment**:
```cpp
// Source: x = 42
// TAC:
x = ASSIGN 42
```

**Compound Assignment**:
```cpp
// Source: x += 5
// TAC:
t1 = x ADD 5
x = ASSIGN t1
```

##### Function Calls
```cpp
TACOperand generate_call(std::shared_ptr<CallExpr> expr);
```

**Example**:
```cpp
// Source: foo(a, b, c)
// TAC:
PARAM a
PARAM b
PARAM c
t1 = CALL foo
// Returns: t1
```

**Member Function Calls** (implicit `this` parameter):
```cpp
// Source: obj.method(x)
// TAC:
t1 = ADDR_OF obj     // 'this' pointer
PARAM t1              // Implicit first parameter
PARAM x
t2 = CALL _ZN3obj6methodE
// Returns: t2
```

##### Ternary Operator
```cpp
TACOperand generate_ternary(std::shared_ptr<TernaryExpr> expr);
```

**Example**:
```cpp
// Source: cond ? a : b
// TAC:
    t_cond = cond
    IF_FALSE t_cond GOTO L_ELSE
    t_result = a
    GOTO L_END
L_ELSE:
    t_result = b
L_END:
    // Returns: t_result
```

---

### Statement Generation

#### Entry Point
```cpp
void generate_statement(ASTNodePtr node);
```

#### Statement Handlers

##### If Statement
```cpp
void generate_if_stmt(std::shared_ptr<IfStmt> stmt);
```

**Without Else**:
```cpp
// Source: if (cond) { body }
// TAC:
    t1 = cond
    IF_FALSE t1 GOTO L_END
    <body instructions>
L_END:
```

**With Else**:
```cpp
// Source: if (cond) { then_body } else { else_body }
// TAC:
    t1 = cond
    IF_FALSE t1 GOTO L_ELSE
    <then_body instructions>
    GOTO L_END
L_ELSE:
    <else_body instructions>
L_END:
```

##### While Loop
```cpp
void generate_while_stmt(std::shared_ptr<WhileStmt> stmt);
```

**Example**:
```cpp
// Source: while (cond) { body }
// TAC:
L_START:
    t1 = cond
    IF_FALSE t1 GOTO L_END
    <body instructions>
    GOTO L_START
L_END:
```

##### For Loop
```cpp
void generate_for_stmt(std::shared_ptr<ForStmt> stmt);
```

**Example**:
```cpp
// Source: for (init; cond; update) { body }
// TAC:
    <init instructions>
L_START:
    t1 = cond
    IF_FALSE t1 GOTO L_END
    <body instructions>
L_CONTINUE:
    <update instructions>
    GOTO L_START
L_END:
```

##### Switch Statement
```cpp
void generate_switch_stmt(std::shared_ptr<SwitchStmt> stmt);
```

**Jump Table Implementation**:
```cpp
// Source: switch (x) { case 1: ...; case 2: ...; default: ... }
// TAC:
    t_switch = x
    JUMP_TABLE t_switch, .JTABLE0  // Uses jump table for efficiency
    GOTO L_DEFAULT                  // Fallback for out-of-range
L_CASE1:
    <case 1 body>
    GOTO L_END
L_CASE2:
    <case 2 body>
    GOTO L_END
L_DEFAULT:
    <default body>
L_END:
```

##### Return Statement
```cpp
void generate_return_stmt(std::shared_ptr<RetExpr> stmt);
```

**Examples**:
```cpp
// Source: return;
// TAC:
RETURN

// Source: return x + 5;
// TAC:
t1 = x ADD 5
RETURN t1
```

##### Block Statement
```cpp
void generate_block_stmt(std::shared_ptr<BlockStmt> stmt);
```

Recursively generates TAC for all statements in the block.

---

### Special Features

#### Loop Control (Break/Continue)

The generator maintains a stack of loop labels:
```cpp
std::stack<std::pair<std::string, std::string>> loop_labels;
// (break_label, continue_label)
```

**Example**:
```cpp
// Source: while (cond) { if (x) break; body; }
// TAC:
L_START:
    t1 = cond
    IF_FALSE t1 GOTO L_BREAK
    t2 = x
    IF_FALSE t2 GOTO L_BODY
    GOTO L_BREAK           // break
L_BODY:
    <body>
    GOTO L_START
L_BREAK:
```

#### Member Access

##### Load Member
```cpp
TACOperand generate_member_access(const TACOperand &base_object,
                                  const std::string &member_name,
                                  TypePtr base_type);
```

**Example**:
```cpp
// Source: obj.x
// TAC:
t1 = LOAD_MEMBER obj, "x", offset=0
// Returns: t1 with member metadata
```

##### Store Member
```cpp
void generate_member_store(const TACOperand &base_object,
                           const std::string &member_name,
                           TypePtr base_type,
                           const TACOperand &value);
```

**Example**:
```cpp
// Source: obj.x = 42
// TAC:
STORE_MEMBER obj, "x", offset=0, value=42
```

##### Nested Member Access
```cpp
void generate_designated_member_store(
    const TACOperand &base_object,
    TypePtr base_type,
    const std::vector<std::string> &member_path,
    const TACOperand &value);
```

**Example**:
```cpp
// Source: obj.inner.field = value
// TAC:
t1 = MEMBER_OFFSET obj, "inner"
STORE_MEMBER t1, "field", offset=X, value
```

#### Aggregate Operations

##### Aggregate Copy
```cpp
void generate_aggregate_copy(const TACOperand &target,
                             const TACOperand &source,
                             TypePtr aggregate_type);
```

Generates word-by-word copy for structs/classes.

**Example**:
```cpp
// Source: struct_a = struct_b;  (8-byte struct)
// TAC:
t1 = LOAD source, offset=0
STORE target, offset=0, t1
```

##### Array Initialization
```cpp
void generate_array_initialization(const TACOperand &base_addr,
                                   TypePtr array_type,
                                   const std::vector<ASTNodePtr> &initializers,
                                   size_t base_offset = 0);
```

**Example**:
```cpp
// Source: int arr[3] = {1, 2, 3};
// TAC:
t_base = ADDR_OF arr
STORE t_base, offset=0, 1
STORE t_base, offset=4, 2
STORE t_base, offset=8, 3
```

---

### Global Variables and Initialization

#### Global Variable Declaration
```cpp
void generate_global_declaration(ASTNodePtr node);
```

Handles:
- Global variable registration
- Constant initializers (stored in symbol)
- Non-constant initializers (deferred to init function)

#### Global Initialization Function
```cpp
void generate_global_init_function();
```

Creates `_GLOBAL__sub_I` function to initialize non-constant globals.

**Example**:
```cpp
// Source: int x = foo();
// Generated init function:
_GLOBAL__sub_I:
    ENTER
    t1 = CALL foo
    x = ASSIGN t1
    RETURN
```

#### Global Finalization Function
```cpp
void generate_global_fini_function();
```

Creates `_GLOBAL__sub_D` function to call destructors for globals.

---

### Function Generation

#### Main Entry Point
```cpp
void generate_function(std::shared_ptr<FunctionDef> func_def);
```

**Steps**:
1. Create `TACFunction` with mangled name
2. Set up parameters (including implicit `this` for methods)
3. Create entry basic block
4. Emit `ENTER` instruction
5. Generate function body TAC
6. Emit `RETURN` instruction (if not already present)
7. Add function to program

**Example Function**:
```cpp
// Source:
int add(int a, int b) {
    return a + b;
}

// Generated TAC:
_ZN3addEii:              // Mangled name: add(int, int)
    ENTER
    t1 = a ADD b
    RETURN t1
```

---

### Helper Functions

#### Temporary Generation
```cpp
std::string new_temp(TypePtr type = nullptr);
```

Delegates to `current_function->new_temp(type)`.

#### Label Generation
```cpp
std::string new_label(const std::string &prefix = "L");
```

Delegates to `current_function->new_label(prefix)`.

#### Instruction Emission
```cpp
void emit(TACInstructionPtr instr);
```

Adds instruction to current basic block and increments instruction counter.

#### Label Emission
```cpp
void emit_label(const std::string &label);
```

Emits a `LABEL` instruction and performs backpatching for forward jumps.

#### Unconditional Jump
```cpp
void emit_goto(const std::string &label);
```

Emits a `GOTO` instruction with backpatching support.

#### Conditional Jump
```cpp
void emit_conditional_jump(TACOpcode opcode,
                           const TACOperand &condition,
                           const std::string &label);
```

Emits `IF_TRUE` or `IF_FALSE` instruction.

---

## Backpatching

TAC supports **forward jumps** (jumps to labels not yet emitted) through a backpatching mechanism.

### Mechanism

1. **Forward Jump**: When a jump to an unknown label is emitted:
   ```cpp
   goto_instr->is_backpatch_target = true;
   current_function->pending_jumps[label].push_back(goto_instr);
   ```

2. **Label Emission**: When the target label is emitted:
   ```cpp
   size_t target_line = current_function->current_instruction_number + 1;
   current_function->emitted_labels[label] = target_line;

   // Backpatch all pending jumps
   for (auto &jump_instr : pending_jumps[label]) {
       jump_instr->target_line_number = target_line;
   }
   ```

3. **Backward Jump**: Resolved immediately:
   ```cpp
   auto it = current_function->emitted_labels.find(label);
   if (it != emitted_labels.end()) {
       goto_instr->target_line_number = it->second;
   }
   ```

---

## Error Handling

### TACError Enum
```cpp
enum class TACError {
    NO_CURRENT_FUNCTION,           // Operation requires function context
    UNHANDLED_EXPRESSION_TYPE,     // Unsupported AST node type
    MEMBER_NOT_FOUND,              // Struct/class member lookup failed
    MEMBER_ACCESS_ON_NON_STRUCT,   // Member access on non-aggregate
    TYPE_DETERMINATION_FAILED,     // Cannot determine operand type
    INVALID_BASE_TYPE,             // Invalid type for operation
    INVALID_OPERAND,               // Malformed operand
};
```

### TACErrorInfo Structure
```cpp
struct TACErrorInfo {
    TACError error_code;
    std::string context;          // Where the error occurred
    std::string additional_info;  // Extra details

    std::string to_string() const;
    CompilerError to_compiler_error() const;
};
```

### Error Recording
```cpp
void record_error(const std::string &message);
```

Errors are accumulated in `std::vector<TACErrorInfo> errors` and returned via `Result<>`.

---

## TAC Output Format

### Text Representation

#### Function Format
```
Function: <name> (<mangled_name>)
Returns: <type>
Parameters: <param_list>

Basic Block: <label>
  <line>: <instruction>
  <line>: <instruction>
  ...
```

#### Instruction Format
```
<line>: [result =] <opcode> [operand1] [, operand2] [; comment]
```

### Example Complete Function

```cpp
// Source:
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
```

**Generated TAC**:
```
Function: factorial (_ZN9factorialEi)
Returns: int
Parameters: n

Basic Block: entry
  0: ENTER
  1: t1 = n LE 1
  2: IF_FALSE t1 GOTO L_ELSE

Basic Block: L_THEN
  3: RETURN 1
  4: GOTO L_END

Basic Block: L_ELSE
  5: t2 = n SUB 1
  6: PARAM t2
  7: t3 = CALL _ZN9factorialEi
  8: t4 = n MUL t3
  9: RETURN t4

Basic Block: L_END
```

---

## Type Information Preservation

TAC preserves type information from the AST for later code generation phases.

### Type in Operands
```cpp
TACOperand op = TACOperand::temporary("t1", int_type);
// op.type points to TypePtr for 'int'
```

### Type-Dependent Operations

#### Integer vs Float
```cpp
// int a, b;
t1 = a ADD b      // Integer addition

// double x, y;
t2 = x ADD y      // Floating-point addition (same opcode, different type)
```

The backend distinguishes these based on `operand.type`.

#### Signed vs Unsigned
```cpp
// unsigned a, b;
t1 = a DIV b      // Unsigned division (type: unsigned int)

// int c, d;
t2 = c DIV d      // Signed division (type: int)
```

#### Pointer Arithmetic
```cpp
// int *ptr; int offset;
t1 = ptr ADD offset    // Pointer arithmetic (type: int*)
// Backend scales offset by sizeof(int)
```

---

## Optimization Opportunities

While TAC generation currently focuses on correctness, the IR enables future optimizations:

### Local Optimizations
- **Constant Folding**: `t1 = 2 ADD 3` → `t1 = 5`
- **Copy Propagation**: Eliminate redundant copies
- **Dead Code Elimination**: Remove unused temporaries
- **Algebraic Simplification**: `t1 = x MUL 0` → `t1 = 0`

### Control Flow Optimizations
- **Unreachable Code Removal**: Eliminate blocks with no predecessors
- **Empty Block Elimination**: Merge blocks with only a jump
- **Branch Optimization**: Simplify conditional chains

### Global Optimizations (require dataflow analysis)
- **Common Subexpression Elimination**
- **Loop Invariant Code Motion**
- **Strength Reduction**
- **Inlining**

---

## Integration with Compilation Pipeline

### Input: AST
The TACGenerator consumes AST nodes produced by the parser, requiring:
- Type-checked AST (all expressions have resolved types)
- Symbol table populated with all declarations
- Scopes and name resolution complete

### Output: TAC Program
The generated `TACProgram` is passed to the code generation backend:
```cpp
TACGenerator gen;
auto result = gen.generate(ast_nodes);

if (result.is_ok()) {
    const TACProgram &tac = gen.get_program();
    RiscV64Codegen codegen(tac, symbol_table, type_factory);
    codegen.emit_to_file("output.s");
} else {
    // Handle errors
    for (const auto &err : result.err_value()) {
        std::cerr << err.to_string() << std::endl;
    }
}
```

---

## Implementation Details

### Virtual Dispatch Translation

**Source** (C++-style virtual call):
```cpp
class Base {
    virtual int foo();
};
Base *obj;
obj->foo();
```

**TAC** (vtable lookup):
```
t1 = LOAD obj              # Load object pointer
t2 = LOAD t1, offset=0     # Load vtable pointer (first field)
t3 = LOAD t2, offset=8     # Load function pointer (vtable[1])
PARAM obj                  # Implicit 'this' parameter
t4 = CALL t3               # Indirect call through function pointer
```

### Array Indexing

**Source**:
```cpp
int arr[10];
int x = arr[i];
```

**TAC**:
```
t1 = ADDR_OF arr           # Base address
t2 = i MUL 4               # Scale by element size (sizeof(int) = 4)
t3 = t1 ADD t2             # Compute element address
x = LOAD t3                # Load array element
```

### Struct Member Access

**Source**:
```cpp
struct Point { int x; int y; };
Point p;
int val = p.y;
```

**TAC**:
```
t1 = LOAD_MEMBER p, "y", offset=4, type=int
val = ASSIGN t1
```

---

## Usage Examples

### Basic Usage

```cpp
#include "tac/tac_generator.hpp"

// Create generator
TACGenerator gen;

// Generate TAC for translation unit
auto result = gen.generate(ast_nodes);

if (result.is_ok()) {
    // Success - get the TAC program
    const TACProgram &program = gen.get_program();

    // Print TAC for debugging
    program.print();

    // Pass to backend
    RiscV64Codegen backend(program, symbol_table, type_factory);
    backend.emit_to_file("output.s");
} else {
    // Handle errors
    for (const auto &error : result.err_value()) {
        std::cerr << "TAC Error: " << error.to_string() << "\n";
    }
}
```

### Generating Single Function

```cpp
TACGenerator gen;

// Manually generate function
auto func_def = /* ... AST function node ... */;
gen.generate_function(func_def);

// Access generated TAC
const TACProgram &program = gen.get_program();
```

### Inspecting Generated TAC

```cpp
const TACProgram &program = gen.get_program();

// Iterate over functions
for (const auto &func : program.functions) {
    std::cout << "Function: " << func->name << "\n";

    // Iterate over basic blocks
    for (const auto &block : func->basic_blocks) {
        std::cout << "  Block: " << block->label << "\n";

        // Iterate over instructions
        for (const auto &instr : block->instructions) {
            std::cout << "    " << instr->to_string() << "\n";
        }
    }
}

// Access global variables
for (const auto &global : program.global_variables) {
    std::cout << "Global: " << global->get_name() << "\n";
}

// Access string literals
for (const auto &str : program.string_literals) {
    std::cout << "String: " << str.label << " = \"" << str.value << "\"\n";
}
```

---

## Design Rationale

### Why Three-Address Code?

1. **Simplicity**: Each instruction performs a single, simple operation
2. **Explicit Temporaries**: No nested expressions, easier to analyze
3. **Architecture Independence**: Not tied to specific hardware
4. **Optimization-Friendly**: Easy to apply transformations
5. **Debuggability**: Human-readable intermediate form

### Why Basic Blocks?

1. **Control Flow Analysis**: Natural unit for CFG construction
2. **Optimization Scope**: Many optimizations operate on basic blocks
3. **Register Allocation**: Live ranges computed per-block
4. **Dataflow Analysis**: Transfer functions operate on blocks

### Design Choices

#### Typed Operands
Every operand carries type information, enabling:
- Type-specific code generation (int vs float instructions)
- Type checking in optimization passes
- Accurate size calculations for memory operations

#### Backpatching for Forward Jumps
Allows single-pass generation of control flow:
- No need for multiple passes over AST
- Efficient label resolution
- Supports arbitrary control flow nesting

#### Mangled Temporaries
Temporaries are mangled with scope information to:
- Avoid naming conflicts across functions
- Enable debugging (map temps back to source scope)
- Support potential temp reuse analysis

---

## Limitations and Future Work

### Current Limitations

1. **No SSA Form**: TAC is not in Static Single Assignment (SSA) form
2. **Limited Optimization**: No optimization passes currently implemented
3. **No Type Inference**: Requires fully type-checked AST input
4. **Linear Structure**: Basic blocks not optimized for analysis
5. **No Exception Handling**: Exception/error control flow not represented

### Future Enhancements

- [ ] Convert to SSA form (add PHI nodes)
- [ ] Implement optimization passes (DCE, CSE, etc.)
- [ ] Add dataflow analysis framework
- [ ] Support for exception handling TAC instructions
- [ ] Better debugging information (source location tracking)
- [ ] TAC interpreter for testing/validation
- [ ] Serialization/deserialization (save/load TAC)

---

## Testing TAC Generation

### Unit Testing Strategy

1. **Expression Tests**: Verify correct lowering of expressions
2. **Control Flow Tests**: Test if/while/for/switch generation
3. **Function Tests**: Parameter passing, return values
4. **Member Access Tests**: Struct/class member operations
5. **Error Handling Tests**: Ensure proper error reporting

### Example Test Case

```cpp
TEST(TACGenerator, BinaryExpressionTest) {
    // Source: a + b * c
    auto mult = std::make_shared<BinaryExpr>(
        Operator::MULTIPLY, b_expr, c_expr
    );
    auto add = std::make_shared<BinaryExpr>(
        Operator::ADD, a_expr, mult
    );

    TACGenerator gen;
    auto result = gen.generate_expression(add);

    const auto &func = gen.get_program().functions[0];
    ASSERT_EQ(func->basic_blocks[0]->instructions.size(), 2);

    // First: t1 = b MUL c
    EXPECT_EQ(func->instructions[0]->opcode, TACOpcode::MUL);

    // Second: t2 = a ADD t1
    EXPECT_EQ(func->instructions[1]->opcode, TACOpcode::ADD);
}
```

---

## Debugging Tips

### Print TAC Output
```cpp
program.print();  // Prints entire program with line numbers
```

### Enable Comments
Add comments to instructions during generation:
```cpp
auto instr = std::make_shared<TACInstruction>(
    TACOpcode::ADD, result, op1, op2
);
instr->comment = "Sum of array elements";
emit(instr);
```

### Trace Generation
Add debug output in generation functions:
```cpp
#ifdef DEBUG_TAC
std::cerr << "Generating TAC for node: "
          << ast_node_type_to_string(node->type) << "\n";
#endif
```

---

## References

- [Tiger Book - Intermediate Representation](https://www.cs.princeton.edu/~appel/modern/c/)
- [Engineering a Compiler - IR Design](https://www.elsevier.com/books/engineering-a-compiler/cooper/978-0-12-088478-0)
- [LLVM IR](https://llvm.org/docs/LangRef.html) - Modern IR for comparison
- [SSA Book](https://ssabook.gforge.inria.fr/latest/) - Static Single Assignment form

---

## Glossary

- **TAC**: Three-Address Code - linearized IR with max 3 operands per instruction
- **Basic Block**: Maximal sequence of instructions with single entry/exit
- **CFG**: Control Flow Graph - directed graph of basic blocks
- **Operand**: Value used or produced by instruction (symbol/temp/constant/label)
- **Temporary**: Compiler-generated variable holding intermediate results
- **Backpatching**: Resolving forward jump targets after label emission
- **Mangling**: Name encoding to avoid conflicts (e.g., `_ZN3fooEi`)
- **SSA**: Static Single Assignment - IR where each variable assigned exactly once
- **Liveness**: Property indicating whether a value is used later in the program
- **Dataflow Analysis**: Technique for gathering information about program behavior

