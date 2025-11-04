#include "codegen/riscv32_codegen.hpp"
#include "codegen/riscv32_backend.hpp"
#include <fstream>
#include <ios>

namespace ciel {
namespace codegen {
namespace riscv32 {

RiscV32Codegen::RiscV32Codegen(const TACProgram &program,
                               SymbolTable &symtab,
                               TypeFactory &types)
    : backend_(std::make_unique<RiscV32Backend>(program, symtab, types))
{
}

RiscV32Codegen::~RiscV32Codegen() = default;

void RiscV32Codegen::emit(std::ostream &os)
{
    backend_->emit(os);
}

void RiscV32Codegen::emit_to_file(const std::string &path)
{
    std::ofstream ofs(path, std::ios::out | std::ios::trunc);
    if (!ofs) {
        throw std::runtime_error("Failed to open output file: " + path);
    }
    emit(ofs);
}

} // namespace riscv32
} // namespace codegen
} // namespace ciel
