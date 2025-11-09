#ifndef CIEL_CODEGEN_RISCV32_BACKEND_HPP
#define CIEL_CODEGEN_RISCV32_BACKEND_HPP

#include "codegen/riscv32_frame.hpp"
#include "codegen/riscv32_instruction.hpp"
#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include "tac/tac.hpp"
#include <iosfwd>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

namespace ciel {
namespace codegen {
namespace riscv32 {

inline constexpr size_t MAX_INT_REGS = 8;
inline constexpr size_t MAX_FLOAT_REGS = 8;
inline constexpr size_t POINTER_SIZE = 8;
inline constexpr size_t WORD_SIZE = 8;

class RiscV32Backend;

class MachineFunction {
  public:
    explicit MachineFunction(const TACFunction &tac_fn, TypeFactory &types);

    [[nodiscard]] const std::string &name() const noexcept
    {
        return name_;
    }

    [[nodiscard]] const auto &instructions() const noexcept
    {
        return instructions_;
    }

    [[nodiscard]] auto &instructions() noexcept
    {
        return instructions_;
    }

    [[nodiscard]] const auto &basic_blocks() const noexcept
    {
        return basic_blocks_;
    }

    [[nodiscard]] auto &frame() noexcept
    {
        return frame_;
    }

    [[nodiscard]] const auto &frame() const noexcept
    {
        return frame_;
    }

    // Legacy accessors for compatibility
    [[nodiscard]] const std::string &get_name() const noexcept
    {
        return name_;
    }
    [[nodiscard]] const std::vector<MachineInstr> &
    get_instructions() const noexcept
    {
        return instructions_;
    }
    [[nodiscard]] std::vector<MachineInstr> &get_instructions() noexcept
    {
        return instructions_;
    }
    [[nodiscard]] const std::vector<MachineBasicBlock> &
    get_basic_blocks() const noexcept
    {
        return basic_blocks_;
    }
    [[nodiscard]] FrameLayout &get_frame() noexcept
    {
        return frame_;
    }
    [[nodiscard]] const FrameLayout &get_frame() const noexcept
    {
        return frame_;
    }

    void add_instruction(MachineInstr instr)
    {
        instructions_.push_back(std::move(instr));
    }
    [[nodiscard]] VirtReg get_next_vreg() noexcept
    {
        return next_vreg_++;
    }

    void build_cfg();

  private:
    std::string name_;
    std::vector<MachineInstr> instructions_;
    std::vector<MachineBasicBlock> basic_blocks_;
    FrameLayout frame_;
    VirtReg next_vreg_;
    TypeFactory &types_;
};

/// Instruction selector: lowers TAC to machine instructions
class InstructionSelector {
  public:
    InstructionSelector(MachineFunction &mfn,
                        TypeFactory &types,
                        RiscV32Backend &backend);

    void select(const TACFunction &tac_fn);

  private:
    struct ParamAllocator {
        size_t int_idx = 0;
        size_t float_idx = 0;
        size_t stack_idx = 0;

        [[nodiscard]] PhysReg next_int_reg() noexcept
        {
            return static_cast<PhysReg>(static_cast<uint8_t>(PhysReg::A0) +
                                        int_idx++);
        }

        [[nodiscard]] PhysReg next_float_reg() noexcept
        {
            return static_cast<PhysReg>(static_cast<uint8_t>(PhysReg::FA0) +
                                        float_idx++);
        }

        [[nodiscard]] int32_t next_stack_offset() noexcept
        {
            return static_cast<int32_t>(stack_idx++ * 8);
        }
    };

    void select_instruction(const TACInstruction &tac_instr);

    void store_param_from_register(PhysReg source_reg,
                                   int32_t stack_offset,
                                   MachineOpcode store_op);
    void store_param_from_stack(int32_t source_stack_offset,
                                int32_t dest_stack_offset,
                                MachineOpcode load_op,
                                MachineOpcode store_op);

    void handle_implicit_this_param(const TACOperand &param,
                                    ParamAllocator &allocator);
    void handle_aggregate_param(SymbolPtr sym,
                                int32_t offset,
                                ParamAllocator &allocator);
    void handle_regular_param(const TACOperand &param,
                              SymbolPtr sym,
                              int32_t offset,
                              ParamAllocator &allocator);

    void select_enter(const TACInstruction &instr);
    void select_return(const TACInstruction &instr);
    void select_assign(const TACInstruction &instr);
    void select_addr_of(const TACInstruction &instr);
    void select_deref(const TACInstruction &instr);
    void select_unary_op(const TACInstruction &instr);
    void select_binary_op(const TACInstruction &instr);
    void select_comparison(const TACInstruction &instr);
    void select_goto(const TACInstruction &instr);
    void select_if_branch(const TACInstruction &instr);
    void select_param(const TACInstruction &instr);
    void select_call(const TACInstruction &instr);
    void select_jump_table(const TACInstruction &instr);
    void select_load(const TACInstruction &instr);
    void select_store(const TACInstruction &instr);
    void select_cast(const TACInstruction &instr);
    void select_load_member(const TACInstruction &instr);
    void select_store_member(const TACInstruction &instr);

    VirtReg compute_member_address(const TACOperand &base_operand,
                                   size_t offset);

    void copy_aggregate_words(VirtReg dst_base,
                              VirtReg src_base,
                              uint32_t size_bytes);

    VirtReg load_operand(const TACOperand &operand);

    void store_result(VirtReg vreg, const TACOperand &dest);

    uint32_t get_type_size(TypePtr type) const;

    bool is_signed_type(TypePtr type) const;

    bool is_float_type(TypePtr type) const;

    bool is_aggregate_type(TypePtr type) const;

    VirtReg get_or_create_vreg_for_temp(const std::string &temp_name);

    int32_t allocate_local_variable(SymbolPtr sym);

    int32_t get_local_variable_offset(SymbolPtr sym);

    MachineOpcode get_load_opcode(TypePtr type, bool is_float) const;
    MachineOpcode get_store_opcode(TypePtr type, bool is_float) const;

    MachineFunction &mfn_;
    TypeFactory &types_;
    RiscV32Backend &backend_;

    std::unordered_map<std::string, VirtReg> temp_to_vreg_;

    // Map from temporary name to stack offset for temporary parameters
    std::unordered_map<std::string, int32_t> temp_to_offset_;

    // Map from symbol name to stack offset for local variables
    std::unordered_map<std::string, int32_t> local_var_offsets_;

    // Parameter symbols -> argument register mapping (a0-a7)
    std::unordered_map<std::string, PhysReg> param_to_reg_;

    // Track aggregate parameters (passed by pointer, need indirection)
    std::unordered_set<std::string> aggregate_params_;

    // Pending parameters for next CALL (vreg + type info)
    std::vector<std::pair<VirtReg, TACOperand>> pending_params_;

    // Track whether each vreg holds a float value
    std::unordered_set<VirtReg> float_vregs_;

    // Track if current function returns an aggregate (needs sret pointer in a0)
    VirtReg sret_ptr_vreg_ = INVALID_VREG;
};

/// RV32 backend: orchestrates instruction selection, register allocation,
/// emission
class RiscV32Backend {
  public:
    RiscV32Backend(const TACProgram &program,
                   SymbolTable &symtab,
                   TypeFactory &types);

    /// Emit complete assembly to output stream
    void emit(std::ostream &os);

    std::string add_float_constant(double value);

    std::string add_jump_table(const std::vector<std::string> &labels);

  private:
    /// Emit assembly preamble (.option, etc.)
    void emit_preamble(std::ostream &os) const;

    /// Emit libc function aliases (mangled -> unmangled)
    void emit_libc_aliases(std::ostream &os) const;

    /// Emit .rodata section (string literals)
    void emit_rodata(std::ostream &os) const;

    /// Emit .bss/.data sections (global variables)
    void emit_globals(std::ostream &os) const;

    /// Emit .text section (functions)
    void emit_text(std::ostream &os);

    /// Emit a single function with prologue/epilogue
    void emit_function(std::ostream &os, MachineFunction &mfn);

    /// Emit function prologue (save regs, allocate frame)
    void emit_prologue(std::ostream &os, const MachineFunction &mfn) const;

    /// Emit function epilogue (restore regs, return)
    void emit_epilogue(std::ostream &os,
                       const MachineFunction &mfn) const noexcept;

    /// Emit a single machine instruction
    void emit_instruction(std::ostream &os,
                          const MachineInstr &instr,
                          const MachineFunction &mfn) const;

    /// Lower TAC functions to machine functions
    void lower_functions();

    const TACProgram &program_;
    SymbolTable &symtab_;
    TypeFactory &types_;

    // Machine functions (post instruction selection + regalloc)
    std::vector<std::unique_ptr<MachineFunction>> machine_functions_;

    std::unordered_map<uint64_t, std::string> float_constants_;
    uint32_t next_float_constant_id_ = 0;

    // Jump tables: label -> list of case labels
    std::vector<std::pair<std::string, std::vector<std::string>>> jump_tables_;
    uint32_t next_jump_table_id_ = 0;
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_BACKEND_HPP
