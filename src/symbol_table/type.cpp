#include "symbol_table/type.hpp"

std::optional<TypePtr>
TypeFactory::lookup_type(const std::string &name,
                         const std::vector<size_t> &scope_chain) const
{
    for (auto it = scope_chain.rbegin(); it != scope_chain.rend(); ++it) {
        auto type_opt = get_type_by_name(name, *it);
        if (type_opt.has_value()) {
            return type_opt;
        }
    }
    return std::nullopt;
}

std::optional<TypePtr> TypeFactory::get_type_by_id(TypeId id) const
{
    auto it = defined_types.find(id);
    if (it != defined_types.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<TypePtr> TypeFactory::get_type_by_name(const std::string &name,
                                                     size_t scope_id) const
{
    auto it = type_names.find({name, scope_id});
    if (it != type_names.end()) {
        return get_type_by_id(it->second);
    }
    return std::nullopt;
}

bool Type::has_qualifier(TypeQualifier qualifier) const
{
    using U = std::underlying_type_t<TypeQualifier>;
    return (static_cast<U>(type_qualifier) & static_cast<U>(qualifier)) ==
           static_cast<U>(qualifier);
}

std::string type_category_to_string(TypeCategory category)
{
    switch (category) {
    case TypeCategory::PRIMITIVE:
        return "Primitive";
    case TypeCategory::STRUCT:
        return "Struct";
    case TypeCategory::CLASS:
        return "Class";
    case TypeCategory::ENUM:
        return "Enum";
    case TypeCategory::ENUM_CONSTANT:
        return "Enum Constant";
    case TypeCategory::UNION:
        return "Union";
    case TypeCategory::TYPEDEF:
        return "Typedef";
    case TypeCategory::ARRAY:
        return "Array";
    case TypeCategory::POINTER:
        return "Pointer";
    case TypeCategory::FUNCTION:
        return "Function";
    case TypeCategory::LABEL:
        return "Label";
    default:
        return "Unknown";
    }
}

std::string type_qualifier_to_string(TypeQualifier qualifier)
{
    switch (qualifier) {
    case TypeQualifier::NONE:
        return "None";
    case TypeQualifier::CONST:
        return "Const";
    case TypeQualifier::VOLATILE:
        return "Volatile";
    case TypeQualifier::CONST_VOLATILE:
        return "Const Volatile";
    default:
        return "Unknown";
    }
}

std::string access_specifier_to_string(AccessSpecifier access)
{
    switch (access) {
    case AccessSpecifier::PUBLIC:
        return "Public";
    case AccessSpecifier::PRIVATE:
        return "Private";
    case AccessSpecifier::PROTECTED:
        return "Protected";
    case AccessSpecifier::DEFAULT:
        return "Default";
    default:
        return "Unknown";
    }
}
