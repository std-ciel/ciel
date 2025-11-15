#include "codegen/riscv64_codegen.hpp"
#include "codegen/riscv64_backend.hpp"
#include <fstream>
#include <ios>

namespace ciel {
namespace codegen {
namespace riscv64 {

RiscV64Codegen::RiscV64Codegen(const TACProgram &program,
                               SymbolTable &symtab,
                               TypeFactory &types)
    : backend_(std::make_unique<RiscV64Backend>(program, symtab, types))
{
}

RiscV64Codegen::~RiscV64Codegen() = default;

void RiscV64Codegen::emit(std::ostream &os)
{
    backend_->emit(os);
}

void RiscV64Codegen::emit_to_file(const std::string &path)
{
    std::ofstream ofs(path, std::ios::out | std::ios::trunc);
    if (!ofs) {
        throw std::runtime_error("Failed to open output file: " + path);
    }
    emit(ofs);
}

} // namespace riscv64
} // namespace codegen
} // namespace ciel
