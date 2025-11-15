#ifndef CIEL_CODEGEN_RISCV64_CODEGEN_HPP
#define CIEL_CODEGEN_RISCV64_CODEGEN_HPP

#include "symbol_table/symbol_table.hpp"
#include "symbol_table/type_factory.hpp"
#include "tac/tac.hpp"
#include <iosfwd>
#include <memory>
#include <string>

namespace ciel {
namespace codegen {
namespace riscv64 {

// Forward declaration
class RiscV64Backend;

/// Main code generator for RV64 architecture
/// Entry point for TAC → RV64 assembly translation
class RiscV64Codegen {
  public:
    RiscV64Codegen(const TACProgram &program,
                   SymbolTable &symtab,
                   TypeFactory &types);

    ~RiscV64Codegen();

    // Non-copyable, non-movable (PIMPL idiom with unique_ptr)
    RiscV64Codegen(const RiscV64Codegen &) = delete;
    RiscV64Codegen &operator=(const RiscV64Codegen &) = delete;
    RiscV64Codegen(RiscV64Codegen &&) = delete;
    RiscV64Codegen &operator=(RiscV64Codegen &&) = delete;

    /// Emit assembly to output stream
    void emit(std::ostream &os);

    /// Emit assembly to file
    void emit_to_file(const std::string &path);

  private:
    std::unique_ptr<RiscV64Backend> backend_;
};

} // namespace riscv64
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV64_CODEGEN_HPP
