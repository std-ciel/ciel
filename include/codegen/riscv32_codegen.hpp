#ifndef CIEL_CODEGEN_RISCV32_CODEGEN_HPP
#define CIEL_CODEGEN_RISCV32_CODEGEN_HPP

#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include "tac/tac.hpp"
#include <iosfwd>
#include <memory>
#include <string>

namespace ciel {
namespace codegen {
namespace riscv32 {

// Forward declaration
class RiscV32Backend;

/// Main code generator for RV32 architecture
/// Entry point for TAC → RV32 assembly translation
class RiscV32Codegen {
  public:
    RiscV32Codegen(const TACProgram &program,
                   SymbolTable &symtab,
                   TypeFactory &types);

    ~RiscV32Codegen();

    // Non-copyable, non-movable (PIMPL idiom with unique_ptr)
    RiscV32Codegen(const RiscV32Codegen &) = delete;
    RiscV32Codegen &operator=(const RiscV32Codegen &) = delete;
    RiscV32Codegen(RiscV32Codegen &&) = delete;
    RiscV32Codegen &operator=(RiscV32Codegen &&) = delete;

    /// Emit assembly to output stream
    void emit(std::ostream &os);

    /// Emit assembly to file
    void emit_to_file(const std::string &path);

  private:
    std::unique_ptr<RiscV32Backend> backend_;
};

} // namespace riscv32
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV32_CODEGEN_HPP
