#ifndef CIEL_DRIVER_DRIVER_HPP
#define CIEL_DRIVER_DRIVER_HPP

#include "common/result.hpp"

#include <filesystem>
#include <string>
#include <vector>

namespace ciel::driver {

enum class OutputType { ASSEMBLY, OBJECT, EXECUTABLE };

struct CompilationOptions {
    std::filesystem::path input_file;
    std::filesystem::path output_file;
    OutputType output_type = OutputType::EXECUTABLE;
    bool verbose = false;
    bool keep_temps = false;
    std::vector<std::string> linker_args;
};

class Driver {
  public:
    Driver() = default;

    [[nodiscard]] auto assemble(const std::filesystem::path &asm_file,
                                const std::filesystem::path &obj_file,
                                bool verbose = false) const
        -> Result<bool, std::string>;

    [[nodiscard]] auto link(const std::vector<std::filesystem::path> &obj_files,
                            const std::filesystem::path &output_file,
                            bool verbose = false) const
        -> Result<bool, std::string>;

    [[nodiscard]] auto compile(const CompilationOptions &opts) const
        -> Result<bool, std::string>;

  private:
    static constexpr const char *ASSEMBLER = "riscv64-linux-gnu-as";
    static constexpr const char *LINKER = "riscv64-linux-gnu-gcc";
    static constexpr const char *MARCH = "rv64imfd";
    static constexpr const char *MABI = "lp64d";

    [[nodiscard]] auto run_command(const std::vector<std::string> &args,
                                   bool verbose) const
        -> Result<bool, std::string>;

    [[nodiscard]] auto get_temp_file(const std::string &suffix) const
        -> std::filesystem::path;
};

} // namespace ciel::driver

#endif // CIEL_DRIVER_DRIVER_HPP
