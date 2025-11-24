#include "driver/driver.hpp"

#include <cstdlib>
#include <iostream>
#include <random>
#include <sstream>

namespace ciel::driver {

namespace fs = std::filesystem;

auto Driver::run_command(const std::vector<std::string> &args,
                         bool verbose) const -> Result<bool, std::string>
{
    std::ostringstream cmd;
    for (const auto &arg : args) {
        if (!cmd.str().empty())
            cmd << ' ';
        if (arg.find(' ') != std::string::npos) {
            cmd << '\'' << arg << '\'';
        } else {
            cmd << arg;
        }
    }

    if (verbose) {
        std::cerr << "Running: " << cmd.str() << '\n';
    }

    auto ret = std::system(cmd.str().c_str());
    if (ret != 0) {
        return {"Command failed: " + cmd.str()};
    }

    return {true};
}

auto Driver::assemble(const fs::path &asm_file,
                      const fs::path &obj_file,
                      bool verbose) const -> Result<bool, std::string>
{
    return run_command({ASSEMBLER,
                        "-march=" + std::string(MARCH),
                        "-mabi=" + std::string(MABI),
                        "-o",
                        obj_file.string(),
                        asm_file.string()},
                       verbose);
}

auto Driver::link(const std::vector<fs::path> &obj_files,
                  const fs::path &output_file,
                  bool verbose) const -> Result<bool, std::string>
{
    std::vector<std::string> cmd = {LINKER,
                                    "-march=" + std::string(MARCH),
                                    "-mabi=" + std::string(MABI),
                                    "-static",
                                    "-o",
                                    output_file.string()};

    for (const auto &obj : obj_files) {
        cmd.push_back(obj.string());
    }

    return run_command(cmd, verbose);
}

auto Driver::get_temp_file(const std::string &suffix) const -> fs::path
{
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dis(100000, 999999);

    auto temp_dir = fs::temp_directory_path();
    return temp_dir / ("ciel_" + std::to_string(dis(gen)) + suffix);
}

auto Driver::compile(const CompilationOptions &opts) const
    -> Result<bool, std::string>
{
    if (!fs::exists(opts.input_file)) {
        return {"Input file does not exist: " + opts.input_file.string()};
    }

    fs::path asm_file = opts.input_file;

    if (opts.output_type == OutputType::ASSEMBLY) {
        if (asm_file != opts.output_file && opts.output_file != "") {
            fs::copy_file(asm_file,
                          opts.output_file,
                          fs::copy_options::overwrite_existing);
        }
        return {true};
    }

    fs::path obj_file = opts.output_type == OutputType::OBJECT
                            ? opts.output_file
                            : get_temp_file(".o");

    if (auto result = assemble(asm_file, obj_file, opts.verbose);
        result.is_err()) {
        return result;
    }

    if (opts.output_type == OutputType::OBJECT) {
        return {true};
    }

    if (auto result = link({obj_file}, opts.output_file, opts.verbose);
        result.is_err()) {
        if (!opts.keep_temps && obj_file != opts.output_file) {
            fs::remove(obj_file);
        }
        return result;
    }

    if (!opts.keep_temps && obj_file != opts.output_file) {
        fs::remove(obj_file);
    }

    return {true};
}

} // namespace ciel::driver
