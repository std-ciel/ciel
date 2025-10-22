#ifndef LAYOUT_PASS_HPP
#define LAYOUT_PASS_HPP

#include "symbol_table/type.hpp"
#include "symbol_table/type_factory.hpp"
#include <unordered_set>

class LayoutPass {
  private:
    TypeFactory &type_factory;
    std::unordered_set<TypePtr> visited;

    bool compute_layout(TypePtr type);

    bool compute_record_layout(RecordTypePtr record);

    bool compute_class_layout(ClassTypePtr class_type);

    bool compute_array_layout(ArrayTypePtr array);

    bool compute_enum_layout(EnumTypePtr enum_type);

  public:
    explicit LayoutPass(TypeFactory &tf) : type_factory(tf) {}

    bool run();

    bool compute_type_layout(TypePtr type);
};

#endif // LAYOUT_PASS_HPP
