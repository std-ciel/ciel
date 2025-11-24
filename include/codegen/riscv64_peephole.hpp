#ifndef CIEL_CODEGEN_RISCV64_PEEPHOLE_HPP
#define CIEL_CODEGEN_RISCV64_PEEPHOLE_HPP

#include "codegen/riscv64_backend.hpp"

namespace ciel {
namespace codegen {
namespace riscv64 {

class PeepholeOptimizer {
  public:
    explicit PeepholeOptimizer(MachineFunction &mfn);

    void run();

  private:
    MachineFunction &mfn_;

    bool optimize_redundant_moves();
    bool optimize_redundant_loads();
    bool optimize_jumps();
    bool optimize_arithmetic();
};

} // namespace riscv64
} // namespace codegen
} // namespace ciel

#endif // CIEL_CODEGEN_RISCV64_PEEPHOLE_HPP
