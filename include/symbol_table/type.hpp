#ifndef TYPE_HPP
#define TYPE_HPP

#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

struct Type;
using TypePtr = std::shared_ptr<Type>;

enum class TypeKind {
    BUILTIN,
    POINTER,
    ARRAY,
    FUNCTION,
    RECORD,
    ENUM,
    CLASS,
    TYPEDEF
};

enum class Qualifier { NONE = 0, CONST = 1, VOLATILE = 2, CONST_VOLATILE = 3 };

enum class BuiltinTypeKind {
    VOID,
    BOOL,
    CHAR,
    INT,
    FLOAT,
    UNSIGNED,
    SIGNED,
    LABEL
};

enum class Access { PUBLIC, PROTECTED, PRIVATE };

struct BaseSpecifier {
    TypePtr base_type;
    Access access;

    // Default constructor
    BaseSpecifier() : base_type(nullptr), access(Access::PRIVATE) {}

    BaseSpecifier(TypePtr base_type, Access access)
        : base_type(std::move(base_type)), access(access)
    {
    }
};

struct Type {
    TypeKind kind;
    Type() = default;
    explicit Type(TypeKind kind) : kind(kind) {}
    virtual ~Type() = default;
    virtual std::string debug_name() const = 0;
    virtual std::string mangled_name() const = 0;
};

struct QualifiedType {
    TypePtr type;
    Qualifier qualifier;

    // Default constructor (needed for std::unordered_map)
    QualifiedType() : type(nullptr), qualifier(Qualifier::NONE) {}

    QualifiedType(TypePtr type)
        : type(std::move(type)), qualifier(Qualifier::NONE)
    {
    }

    QualifiedType(TypePtr type, Qualifier qualifier)
        : type(std::move(type)), qualifier(qualifier)
    {
    }

    bool operator==(const QualifiedType &other) const
    {
        return type == other.type && qualifier == other.qualifier;
    }

    std::string debug_name() const
    {
        if (!type)
            return "invalid";
        std::string q;
        if (qualifier == Qualifier::CONST)
            q = " const";
        else if (qualifier == Qualifier::VOLATILE)
            q = " volatile";
        else if (qualifier == Qualifier::CONST_VOLATILE)
            q = " const volatile";
        return type->debug_name() + q;
    }

    std::string mangled_name() const
    {
        if (!type)
            return "v"; // invalid -> void

        std::string base = type->mangled_name();

        // Apply CV-qualifiers according to Itanium ABI
        if (qualifier == Qualifier::CONST)
            return "K" + base;
        else if (qualifier == Qualifier::VOLATILE)
            return "V" + base;
        else if (qualifier == Qualifier::CONST_VOLATILE)
            return "VK" + base; // volatile const (order matters)

        return base;
    }
};

struct MemberInfo {
    QualifiedType type;
    Access access;
    bool is_static;

    // Default constructor (needed for std::unordered_map)
    MemberInfo() : type(), access(Access::PRIVATE), is_static(false) {}

    MemberInfo(QualifiedType type, Access access, bool is_static)
        : type(std::move(type)), access(access), is_static(is_static)
    {
    }
};

struct BuiltinType : public Type {
    BuiltinTypeKind builtin_kind;

    explicit BuiltinType(BuiltinTypeKind kind)
        : Type(TypeKind::BUILTIN), builtin_kind(kind)
    {
    }

    std::string debug_name() const override
    {
        switch (builtin_kind) {
        case BuiltinTypeKind::VOID:
            return "void";
        case BuiltinTypeKind::BOOL:
            return "bool";
        case BuiltinTypeKind::CHAR:
            return "char";
        case BuiltinTypeKind::INT:
            return "int";
        case BuiltinTypeKind::FLOAT:
            return "float";
        case BuiltinTypeKind::UNSIGNED:
            return "unsigned";
        case BuiltinTypeKind::SIGNED:
            return "signed";
        case BuiltinTypeKind::LABEL:
            return "label";
        default:
            return "unknown_builtin";
        }
    }

    std::string mangled_name() const override
    {
        switch (builtin_kind) {
        case BuiltinTypeKind::VOID:
            return "v";
        case BuiltinTypeKind::BOOL:
            return "b";
        case BuiltinTypeKind::CHAR:
            return "c";
        case BuiltinTypeKind::INT:
            return "i";
        case BuiltinTypeKind::FLOAT:
            return "f";
        case BuiltinTypeKind::UNSIGNED:
            return "j"; // unsigned int
        case BuiltinTypeKind::SIGNED:
            return "i"; // signed int (same as int)
        default:
            return "u"; // unknown type
        }
    }
};

struct PointerType : public Type {
    QualifiedType pointee;

    explicit PointerType(QualifiedType pointee)
        : Type(TypeKind::POINTER), pointee(std::move(pointee))
    {
    }

    std::string debug_name() const override
    {
        return pointee.debug_name() + "*";
    }

    std::string mangled_name() const override
    {
        return "P" + pointee.mangled_name();
    }
};

struct ArrayType : public Type {
    QualifiedType element_type;
    size_t size;

    ArrayType(QualifiedType element_type, size_t size)
        : Type(TypeKind::ARRAY), element_type(std::move(element_type)),
          size(size)
    {
    }

    std::string debug_name() const override
    {
        return element_type.debug_name() + "[" +
               (size ? std::to_string(size) : "") + "]";
    }

    std::string mangled_name() const override
    {
        if (size) {
            // Array with known size: A<size>_<element-type>
            return "A" + std::to_string(size) + "_" +
                   element_type.mangled_name();
        } else {
            // Array with unknown size: A_<element-type>
            return "A_" + element_type.mangled_name();
        }
    }
};

struct FunctionType : public Type {
    QualifiedType return_type;
    std::vector<QualifiedType> param_types;
    bool is_variadic;

    FunctionType() = default;

    FunctionType(QualifiedType return_type,
                 std::vector<QualifiedType> param_types,
                 bool is_variadic = false)
        : Type(TypeKind::FUNCTION), return_type(std::move(return_type)),
          param_types(std::move(param_types)), is_variadic(is_variadic)
    {
    }

    std::string debug_name() const override
    {
        std::string name = return_type.debug_name() + " (";
        for (size_t i = 0; i < param_types.size(); ++i) {
            if (i > 0)
                name += ", ";
            name += param_types[i].debug_name();
        }
        if (is_variadic) {
            if (!param_types.empty())
                name += ", ";
            name += "...";
        }
        name += ")";
        return name;
    }

    [[deprecated("This function is not meant to be used. Please use "
                 "mangle_function_name function instead.")]] std::string
    mangled_name() const override
    {
        return ""; // Not to be used
    }
};

struct RecordType : public Type {
    std::string tag;
    std::unordered_map<std::string, QualifiedType> fields;
    bool is_union;
    bool is_defined;
    RecordType(std::string tag, bool is_union = false, bool is_defined = false)
        : Type(TypeKind::RECORD), tag(std::move(tag)), is_union(is_union),
          is_defined(is_defined)
    {
    }
    void add_field(const std::string &name, QualifiedType type)
    {
        fields[name] = std::move(type);
    }
    std::string debug_name() const override
    {
        return (is_union ? "union " : "struct ") + tag;
    }

    std::string mangled_name() const override
    {
        // Record type: <tag-length><tag-name>
        return std::to_string(tag.length()) + tag;
    }
};

struct EnumType : public Type {
    std::string tag;
    std::unordered_map<std::string, int64_t> enumerators;
    bool is_defined;

    EnumType(std::string tag, bool is_defined = false)
        : Type(TypeKind::ENUM), tag(std::move(tag)), is_defined(is_defined)
    {
    }
    void add_enumerator(const std::string &name, int64_t value)
    {
        enumerators[name] = value;
    }
    std::string debug_name() const override
    {
        return "enum " + tag;
    }

    std::string mangled_name() const override
    {
        // Enum type: <tag-length><tag-name>
        return std::to_string(tag.length()) + tag;
    }
};

struct ClassType : public Type {
    std::string name;
    std::unordered_map<std::string, MemberInfo> members;
    BaseSpecifier base;
    bool is_defined;

    ClassType(std::string name,
              TypePtr base_type,
              Access base_access,
              bool is_defined = false)
        : Type(TypeKind::CLASS), name(std::move(name)),
          base(base_type, base_access), is_defined(is_defined)
    {
    }

    void add_member(const std::string &name, MemberInfo member)
    {
        members[name] = std::move(member);
    }

    std::string debug_name() const override
    {
        return "class " + name;
    }

    std::string mangled_name() const override
    {
        // Class type: <name-length><name>
        return std::to_string(name.length()) + name;
    }
};

struct TypedefType : public Type {
    std::string name;
    QualifiedType underlying_type;

    TypedefType(std::string name, QualifiedType underlying_type)
        : Type(TypeKind::TYPEDEF), name(std::move(name)),
          underlying_type(std::move(underlying_type))
    {
    }

    std::string debug_name() const override
    {
        return "typedef " + name;
    }

    std::string mangled_name() const override
    {
        // For typedef, delegate to the underlying type's mangling
        return underlying_type.mangled_name();
    }
};

std::string type_kind_to_string(TypeKind kind);

TypePtr strip_typedefs(TypePtr type);
bool is_pointer_type(TypePtr type);
bool is_integral_type(TypePtr type);
bool is_floating_type(TypePtr type);
bool is_arithmetic_type(TypePtr type);
bool is_scalar_type(TypePtr type);
bool is_class_type(TypePtr type);
bool is_bool_type(TypePtr type);
bool is_void_type(TypePtr type);
bool are_types_equal(TypePtr a, TypePtr b);
bool is_integral_or_enum_non_bool(TypePtr type);
bool is_user_defined_type(TypePtr type);
bool is_complete_type(TypePtr type);

using TypeId = size_t;

using BuiltinTypePtr = std::shared_ptr<BuiltinType>;
using PointerTypePtr = std::shared_ptr<PointerType>;
using ArrayTypePtr = std::shared_ptr<ArrayType>;
using FunctionTypePtr = std::shared_ptr<FunctionType>;
using RecordTypePtr = std::shared_ptr<RecordType>;
using EnumTypePtr = std::shared_ptr<EnumType>;
using ClassTypePtr = std::shared_ptr<ClassType>;
using TypedefTypePtr = std::shared_ptr<TypedefType>;

#endif // TYPE_HPP
