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

    /// Select instructions for a TAC function
    void select(const TACFunction &tac_fn);

  private:
    /// Select instruction for a single TAC instruction
    void select_instruction(const TACInstruction &tac_instr);

    /// Helper methods for specific TAC opcodes
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

    VirtReg load_operand(const TACOperand &operand);

    void store_result(VirtReg vreg, const TACOperand &dest);

    uint32_t get_type_size(TypePtr type) const;

    bool is_signed_type(TypePtr type) const;

    bool is_float_type(TypePtr type) const;

    VirtReg get_or_create_vreg_for_temp(const std::string &temp_name);

    MachineFunction &mfn_;
    TypeFactory &types_;
    RiscV32Backend &backend_;

    std::unordered_map<std::string, VirtReg> temp_to_vreg_;

    // Parameter symbols -> argument register mapping (a0-a7)
    std::unordered_map<std::string, PhysReg> param_to_reg_;

    // Pending parameters for next CALL
    std::vector<VirtReg> pending_params_;

    // Track whether each vreg holds a float value
    std::unordered_set<VirtReg> float_vregs_;
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

  private:
    /// Emit assembly preamble (.option, etc.)
    void emit_preamble(std::ostream &os) const;

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
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_BACKEND_HPP
