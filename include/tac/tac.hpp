#ifndef TAC_HPP
#define TAC_HPP

#include "symbol_table/symbol.hpp"
#include "symbol_table/type.hpp"
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

// Forward declarations
struct TACInstruction;
using TACInstructionPtr = std::shared_ptr<TACInstruction>;
class SymbolTable;

enum class TACOpcode {
    // Arithmetic
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    NEG,

    // Bitwise
    AND,
    OR,
    XOR,
    NOT,
    SHL,
    SHR,

    // Logical
    LAND,
    LOR,
    LNOT,

    // Relational
    EQ,
    NE,
    LT,
    LE,
    GT,
    GE,

    // Assignment
    ASSIGN,
    COPY,

    // Memory
    LOAD,
    STORE,
    ADDR_OF,
    DEREF,

    // Control Flow
    LABEL,
    GOTO,
    IF_FALSE,
    IF_TRUE,

    // Function
    PARAM,
    CALL,
    RETURN,
    ENTER,
    LEAVE,

    // Array/Member Access
    INDEX_ADDR,
    MEMBER_ADDR,

    // NEW: Struct/Class operations
    MEMBER_ACCESS, // Access struct/class member
    LOAD_MEMBER,   // Load from struct member: result = base.member
    STORE_MEMBER,  // Store to struct member: base.member = value
    MEMBER_OFFSET, // Calculate member offset: result = &base + offset

    // Type Operations
    CAST,

    // Jump Table Operations
    JUMP_TABLE,      // Jump using a table: JUMP_TABLE index, table_label
    JUMP_TABLE_INIT, // Initialize jump table metadata

    // Special
    NOP,
    PHI
};

// TAC Operand can be a symbol, temporary, constant, or label
struct TACOperand {
    enum class Kind { SYMBOL, TEMPORARY, CONSTANT, LABEL, NONE };

    Kind kind;
    std::variant<SymbolPtr, std::string, int64_t, double> value;
    TypePtr type;

    // NEW: Additional fields for struct member access
    std::string member_name; // For member access operations
    size_t member_offset;    // Byte offset of member in struct

    TACOperand() : kind(Kind::NONE), type(nullptr), member_offset(0) {}

    static TACOperand symbol(SymbolPtr sym, TypePtr t = nullptr);
    static TACOperand temporary(const std::string &name, TypePtr t);
    static TACOperand constant_int(int64_t val, TypePtr t);
    static TACOperand constant_float(double val, TypePtr t);
    static TACOperand label(const std::string &name);

    // NEW: Create operand with member information
    static TACOperand member_access(const TACOperand &base,
                                    const std::string &member,
                                    size_t offset,
                                    TypePtr member_type);

    std::string to_string() const;
    bool is_valid() const
    {
        return kind != Kind::NONE;
    }
};

// Three Address Code Instruction
struct TACInstruction {
    TACOpcode opcode;
    TACOperand result;   // destination
    TACOperand operand1; // first source
    TACOperand operand2; // second source (optional)

    std::string comment; // for debugging

    // For jump table instructions
    std::vector<std::string> jump_table_labels;
    int64_t jump_table_min = 0;
    int64_t jump_table_max = 0;

    // For backpatching: store target line number for goto/conditional jumps
    size_t target_line_number = 0;
    bool is_backpatch_target = false; // true if this needs backpatching

    TACInstruction(TACOpcode op,
                   TACOperand res = TACOperand(),
                   TACOperand op1 = TACOperand(),
                   TACOperand op2 = TACOperand())
        : opcode(op), result(res), operand1(op1), operand2(op2)
    {
    }

    std::string to_string(size_t line_offset = 0) const;
};

// Basic Block for organizing TAC instructions
struct TACBasicBlock : public std::enable_shared_from_this<TACBasicBlock> {
    std::string label;
    std::vector<TACInstructionPtr> instructions;
    std::vector<std::shared_ptr<TACBasicBlock>> successors;
    std::vector<std::shared_ptr<TACBasicBlock>> predecessors;

    explicit TACBasicBlock(const std::string &lbl = "") : label(lbl) {}

    void add_instruction(TACInstructionPtr instr)
    {
        instructions.push_back(instr);
    }

    void add_successor(std::shared_ptr<TACBasicBlock> succ)
    {
        successors.push_back(succ);
        succ->predecessors.push_back(shared_from_this());
    }
};

using TACBasicBlockPtr = std::shared_ptr<TACBasicBlock>;

// Function's TAC representation
struct TACFunction {
    std::string name;
    std::string mangled_name;
    TypePtr return_type;
    std::vector<TACOperand> parameters;
    std::vector<TACBasicBlockPtr> basic_blocks;
    TACBasicBlockPtr entry_block;
    TACBasicBlockPtr exit_block;

    int temp_counter = 0;
    int label_counter = 0;

    // For generating mangled temporaries in correct scope
    ScopeID body_scope_id = 0;
    SymbolTable &sym_table;

    // For backpatching during code generation
    size_t current_instruction_number = 0;

    // Map label names to list of instructions that jump to them (forward jumps)
    std::map<std::string, std::vector<TACInstructionPtr>> pending_jumps;

    // Map label names to their line numbers (for backward jumps)
    std::map<std::string, size_t> emitted_labels;

    TACFunction(const std::string &n,
                const std::string &mn,
                TypePtr ret,
                ScopeID body_scope_id,
                SymbolTable &st)
        : name(n), mangled_name(mn), return_type(ret),
          body_scope_id(body_scope_id), sym_table(st)
    {
    }

    std::string new_temp(TypePtr type);
    std::string new_label(const std::string &prefix = "L");
    void add_block(TACBasicBlockPtr block);
    void print(size_t &line_number) const;
};

using TACFunctionPtr = std::shared_ptr<TACFunction>;

// String literal entry
struct StringLiteral {
    std::string label;
    std::string value;

    StringLiteral(const std::string &lbl, const std::string &val)
        : label(lbl), value(val)
    {
    }
};

// Complete TAC Program
struct TACProgram {
    std::vector<TACFunctionPtr> functions;
    std::vector<SymbolPtr> global_variables;
    std::vector<StringLiteral> string_literals;

    void add_function(TACFunctionPtr func)
    {
        functions.push_back(func);
    }

    void add_global(SymbolPtr sym)
    {
        global_variables.push_back(sym);
    }

    void add_string_literal(const std::string &label, const std::string &value)
    {
        string_literals.emplace_back(label, value);
    }

    void print() const;
};

std::string opcode_to_string(TACOpcode op);

#endif // TAC_HPP
