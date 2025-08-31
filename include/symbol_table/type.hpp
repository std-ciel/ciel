#ifndef TYPE_HPP
#define TYPE_HPP

#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

using TypeId = uint64_t;

struct Type;
using TypePtr = std::shared_ptr<Type>;

struct StructType;
struct EnumType;
struct EnumConstant;
struct UnionType;
struct ClassType;
struct FunctionType;
struct ArrayType;
struct TypedefType;
struct PointerType;
struct PrimitiveType;
struct LabelType;

enum class TypeQualifier {
    NONE = 0,
    CONST = 1 << 0,
    VOLATILE = 1 << 1,
    CONST_VOLATILE = CONST | VOLATILE
};

enum class TypeCategory {
    PRIMITIVE,
    STRUCT,
    CLASS,
    ENUM,
    ENUM_CONSTANT,
    UNION,
    TYPEDEF,
    ARRAY,
    POINTER,
    FUNCTION,
    LABEL
};

enum class AccessSpecifier { PUBLIC, PRIVATE, PROTECTED, DEFAULT };

std::string type_category_to_string(TypeCategory category);
std::string type_qualifier_to_string(TypeQualifier qualifier);
std::string access_specifier_to_string(AccessSpecifier access);

struct type_access {
    AccessSpecifier access_specifier;
    TypePtr type;

    type_access(AccessSpecifier access = AccessSpecifier::DEFAULT,
                TypePtr t = nullptr)
        : access_specifier(access), type(t)
    {
    }
};

struct InheritanceInfo {
    TypePtr base_class;
    AccessSpecifier inheritance_access;

    InheritanceInfo(TypePtr base = nullptr,
                    AccessSpecifier access = AccessSpecifier::PUBLIC)
        : base_class(base), inheritance_access(access)
    {
    }
};

struct StructType {
    std::unordered_map<std::string, TypePtr> fields;

    StructType() = default;
    explicit StructType(std::unordered_map<std::string, TypePtr> f)
        : fields(std::move(f))
    {
    }
};

struct EnumConstant {
    int64_t value;
    TypePtr parent_enum;

    EnumConstant(int64_t v = 0, TypePtr p = nullptr) : value(v), parent_enum(p)
    {
    }

    bool operator==(const EnumConstant &other) const
    {
        return parent_enum == other.parent_enum && value == other.value;
    }

    struct Hash {
        size_t operator()(const EnumConstant &constant) const
        {
            return std::hash<int64_t>()(constant.value) ^
                   std::hash<TypePtr>()(constant.parent_enum);
        }
    };
};

struct EnumType {
    std::unordered_set<EnumConstant, EnumConstant::Hash> values;

    EnumType() = default;
    explicit EnumType(std::unordered_set<EnumConstant, EnumConstant::Hash> v)
        : values(std::move(v))
    {
    }
};

struct UnionType {
    std::unordered_map<std::string, TypePtr> fields;

    UnionType() = default;
    explicit UnionType(std::unordered_map<std::string, TypePtr> f)
        : fields(std::move(f))
    {
    }
};

struct ClassType {
    std::unordered_map<std::string, type_access> fields;
    std::unordered_map<std::string, type_access> methods;
    std::optional<InheritanceInfo> inheritance;
    std::unordered_map<std::string, TypePtr> operator_overloads;

    ClassType() = default;
    explicit ClassType(std::unordered_map<std::string, type_access> f)
        : fields(std::move(f))
    {
    }

    void set_base_class(TypePtr base,
                        AccessSpecifier access = AccessSpecifier::PRIVATE)
    {
        inheritance = InheritanceInfo(base, access);
    }

    void add_operator_overload(const std::string &op, TypePtr func_type)
    {
        operator_overloads[op] = func_type;
    }
};

struct FunctionType {
    enum class FunctionKind {
        NORMAL,
        CONSTRUCTOR,
        DESTRUCTOR,
        OPERATOR_OVERLOAD
    };

    TypePtr return_type;
    std::vector<TypePtr> param_types;
    std::vector<std::string> param_names;
    bool is_variadic;
    FunctionKind kind = FunctionKind::NORMAL;
    TypePtr parent_class = nullptr;
    std::string operator_name;

    FunctionType(TypePtr ret = nullptr,
                 std::vector<TypePtr> types = {},
                 bool variadic = false,
                 FunctionKind k = FunctionKind::NORMAL,
                 TypePtr parent = nullptr)
        : return_type(ret), param_types(std::move(types)), param_names(),
          is_variadic(variadic), kind(k), parent_class(parent)
    {
        param_names.resize(param_types.size());
    }

    FunctionType(TypePtr ret,
                 std::vector<TypePtr> types,
                 std::vector<std::string> names,
                 bool variadic = false,
                 FunctionKind k = FunctionKind::NORMAL,
                 TypePtr parent = nullptr)
        : return_type(ret), param_types(std::move(types)),
          param_names(std::move(names)), is_variadic(variadic), kind(k),
          parent_class(parent)
    {
        if (param_names.size() < param_types.size()) {
            param_names.resize(param_types.size());
        }
    }
};

struct TypedefType {
    TypePtr actual_type;

    explicit TypedefType(TypePtr type = nullptr) : actual_type(type) {}
};

struct ArrayType {
    TypePtr element_type;
    size_t size;

    ArrayType(TypePtr element = nullptr, size_t s = 0)
        : element_type(element), size(s)
    {
    }
};

struct PointerType {
    TypePtr points_to;

    explicit PointerType(TypePtr target = nullptr) : points_to(target) {}
};

struct PrimitiveType {
    enum class Kind {
        VOID,
        INT,
        SIGNED,
        UNSIGNED,
        FLOAT,
        CHAR,
        BOOL,
    } kind;

    explicit PrimitiveType(Kind k = Kind::INT) : kind(k) {}
};

struct LabelType {};

struct Type {
    std::string name;
    TypeId id;
    TypeCategory category;
    TypeQualifier type_qualifier;
    std::variant<PrimitiveType,
                 StructType,
                 EnumType,
                 EnumConstant,
                 UnionType,
                 ClassType,
                 FunctionType,
                 TypedefType,
                 ArrayType,
                 PointerType,
                 LabelType>
        info;

    bool operator==(const Type &other) const
    {
        return id == other.id;
    }

    bool has_qualifier(TypeQualifier qualifier) const;
};

namespace std {
template <> struct hash<std::pair<std::string, size_t>> {
    size_t operator()(const std::pair<std::string, size_t> &p) const
    {
        return std::hash<std::string>()(p.first) ^
               (std::hash<size_t>()(p.second) << 1);
    }
};
} // namespace std

class TypeFactory {
  private:
    std::unordered_map<TypeId, TypePtr> defined_types;
    TypeId next_type_id = 0;
    // name & scope id
    std::unordered_map<std::pair<std::string, size_t>, TypeId> type_names;

    TypePtr create_primitive_type(const std::string &name,
                                  PrimitiveType::Kind kind)
    {
        auto type = std::make_shared<Type>();
        type->name = name;
        type->id = next_type_id;
        type->category = TypeCategory::PRIMITIVE;
        type->type_qualifier = TypeQualifier::NONE;
        type->info = PrimitiveType(kind);
        defined_types[next_type_id] = type;
        type_names[{name, 0}] = next_type_id;
        ++next_type_id;
        return type;
    }

  public:
    TypeFactory() : next_type_id(0)
    {
        create_primitive_type("void", PrimitiveType::Kind::VOID);
        create_primitive_type("int", PrimitiveType::Kind::INT);
        create_primitive_type("signed", PrimitiveType::Kind::SIGNED);
        create_primitive_type("unsigned", PrimitiveType::Kind::UNSIGNED);
        create_primitive_type("float", PrimitiveType::Kind::FLOAT);
        create_primitive_type("char", PrimitiveType::Kind::CHAR);
        create_primitive_type("bool", PrimitiveType::Kind::BOOL);
        (void)make("label",
                   0,
                   TypeCategory::LABEL,
                   TypeQualifier::NONE,
                   LabelType{});
    }

    template <typename T>
    TypePtr make(const std::string &type_name,
                 const size_t scope_id,
                 TypeCategory category = TypeCategory::PRIMITIVE,
                 TypeQualifier qualifier = TypeQualifier::NONE,
                 T &&info_value = T{})
    {
        auto it = type_names.find({type_name, scope_id});
        if (it != type_names.end()) {
            return defined_types[it->second];
        }

        auto new_type = std::make_shared<Type>();
        new_type->name = type_name;
        new_type->id = next_type_id;
        new_type->category = category;
        new_type->type_qualifier = qualifier;
        new_type->info = std::forward<T>(info_value);

        defined_types[next_type_id] = new_type;
        type_names[{type_name, scope_id}] = next_type_id;
        ++next_type_id;
        return new_type;
    }

    TypePtr make_pointer_to(const TypePtr &base_type,
                            size_t scope_id,
                            TypeQualifier qualifier = TypeQualifier::NONE)
    {
        if (!base_type) {
            return nullptr;
        }

        std::string ptr_type_name = base_type->name + "*";
        return make(ptr_type_name,
                    scope_id,
                    TypeCategory::POINTER,
                    qualifier,
                    PointerType(base_type));
    }

    TypePtr make_multi_level_pointer(const TypePtr &base_type,
                                     size_t levels,
                                     size_t scope_id,
                                     std::vector<TypeQualifier> qualifiers = {})
    {
        if (!base_type || levels == 0) {
            return base_type;
        }

        TypePtr current = base_type;
        for (size_t i = 0; i < levels; ++i) {
            current = make_pointer_to(
                current,
                scope_id,
                qualifiers.size() > i ? qualifiers[i] : TypeQualifier::NONE);
        }
        return current;
    }

    TypePtr make_array_of(const TypePtr &base_type,
                          size_t array_size,
                          size_t scope_id,
                          TypeQualifier qualifier = TypeQualifier::NONE)
    {
        if (!base_type) {
            return nullptr;
        }

        std::string array_type_name =
            base_type->name + "[" + std::to_string(array_size) + "]";
        return make(array_type_name,
                    scope_id,
                    TypeCategory::ARRAY,
                    qualifier,
                    ArrayType(base_type, array_size));
    }

    TypePtr
    make_multi_dimensional_array(const TypePtr &base_type,
                                 const std::vector<size_t> &dimensions,
                                 size_t scope_id,
                                 std::vector<TypeQualifier> qualifiers = {})
    {
        if (!base_type || dimensions.empty()) {
            return base_type;
        }

        TypePtr current = base_type;
        for (size_t i = 0; i < dimensions.size(); ++i) {
            current = make_array_of(
                current,
                dimensions[i],
                scope_id,
                qualifiers.size() > i ? qualifiers[i] : TypeQualifier::NONE);
        }
        return current;
    }

    std::optional<TypePtr>
    lookup_type(const std::string &name,
                const std::vector<size_t> &scope_chain) const;

    std::optional<TypePtr> get_type_by_id(TypeId id) const;

    std::optional<TypePtr> get_type_by_name(const std::string &name,
                                            size_t scope_id) const;
    // get defined_types
    const std::unordered_map<TypeId, TypePtr> &get_defined_types() const
    {
        return defined_types;
    }
};

#endif // TYPE_HPP
