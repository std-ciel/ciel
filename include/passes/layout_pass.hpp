#ifndef LAYOUT_PASS_HPP
#define LAYOUT_PASS_HPP

#include "common/result.hpp"
#include "passes/layout_pass_errors.hpp"
#include "symbol_table/type.hpp"
#include "symbol_table/type_factory.hpp"
#include <unordered_set>
#include <vector>

class LayoutPass {
  private:
    TypeFactory &type_factory;
    std::unordered_set<TypePtr> visited;
    std::vector<LayoutPassErrorInfo> errors;

    Result<bool, LayoutPassErrorInfo> compute_layout(TypePtr type);

    Result<bool, LayoutPassErrorInfo>
    compute_record_layout(RecordTypePtr record);

    Result<bool, LayoutPassErrorInfo>
    compute_class_layout(ClassTypePtr class_type);

    Result<bool, LayoutPassErrorInfo> compute_array_layout(ArrayTypePtr array);

    Result<bool, LayoutPassErrorInfo>
    compute_enum_layout(EnumTypePtr enum_type);

  public:
    explicit LayoutPass(TypeFactory &tf) : type_factory(tf) {}

    Result<bool, std::vector<LayoutPassErrorInfo>> run();

    Result<bool, LayoutPassErrorInfo> compute_type_layout(TypePtr type);

    const std::vector<LayoutPassErrorInfo> &get_errors() const
    {
        return errors;
    }
};

#endif // LAYOUT_PASS_HPP
