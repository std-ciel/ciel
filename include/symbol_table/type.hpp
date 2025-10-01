#ifndef TYPE_HPP
#define TYPE_HPP

#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

struct Type;
using TypePtr = std::shared_ptr<Type>;

enum class TypeKind {
    Builtin,
    Pointer,
    Array,
    Function,
    Record,
    Enum,
    Class,
    Typedef
};

enum class Qualifier {
    Q_NONE = 0,
    Q_CONST = 1,
    Q_VOLATILE = 2,
    Q_CONST_VOLATILE = 3
};
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

enum class FunctionKind { NORMAL, METHOD, CONSTRUCTOR, DESTRUCTOR, OPERATOR };

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
    const TypeKind kind;
    explicit Type(TypeKind kind) : kind(kind) {}
    virtual ~Type() = default;
    virtual std::string debug_name() const = 0;
};

struct QualType {
    TypePtr type;
    Qualifier qualifier;

    // Default constructor (needed for std::unordered_map)
    QualType() : type(nullptr), qualifier(Qualifier::Q_NONE) {}

    QualType(TypePtr type, Qualifier qualifier)
        : type(std::move(type)), qualifier(qualifier)
    {
    }

    bool operator==(const QualType &other) const
    {
        return type == other.type && qualifier == other.qualifier;
    }
    std::string debug_name() const
    {
        if (!type)
            return "invalid";
        std::string q;
        if (qualifier == Qualifier::Q_CONST)
            q = " const";
        else if (qualifier == Qualifier::Q_VOLATILE)
            q = " volatile";
        else if (qualifier == Qualifier::Q_CONST_VOLATILE)
            q = " const volatile";
        return type->debug_name() + q;
    }
};

struct MemberInfo {
    QualType type;
    Access access;
    bool is_static;

    // Default constructor (needed for std::unordered_map)
    MemberInfo() : type(), access(Access::PRIVATE), is_static(false) {}

    MemberInfo(QualType type, Access access, bool is_static)
        : type(std::move(type)), access(access), is_static(is_static)
    {
    }
};

struct BuiltinType : public Type {
    BuiltinTypeKind builtin_kind;

    explicit BuiltinType(BuiltinTypeKind kind)
        : Type(TypeKind::Builtin), builtin_kind(kind)
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
};

struct PointerType : public Type {
    QualType pointee;

    explicit PointerType(QualType pointee)
        : Type(TypeKind::Pointer), pointee(std::move(pointee))
    {
    }

    std::string debug_name() const override
    {
        return pointee.debug_name() + "*";
    }
};

struct ArrayType : public Type {
    QualType element_type;
    std::optional<size_t> size;

    ArrayType(QualType element_type, std::optional<size_t> size)
        : Type(TypeKind::Array), element_type(std::move(element_type)),
          size(size)
    {
    }

    std::string debug_name() const override
    {
        return element_type.debug_name() + "[" +
               (size ? std::to_string(*size) : "") + "]";
    }
};

struct FunctionType : public Type {
    QualType return_type;
    std::vector<QualType> param_types;
    bool is_variadic;
    FunctionKind function_kind;

    FunctionType(QualType return_type,
                 std::vector<QualType> param_types,
                 FunctionKind kind,
                 bool is_variadic = false,
                 bool is_member_function = false)
        : Type(TypeKind::Function), return_type(std::move(return_type)),
          param_types(std::move(param_types)), is_variadic(is_variadic),
          function_kind(kind)
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
};

struct RecordType : public Type {
    std::string tag;
    std::unordered_map<std::string, QualType> fields;
    bool is_union;
    bool is_defined;
    RecordType(std::string tag, bool is_union = false, bool is_defined = false)
        : Type(TypeKind::Record), tag(std::move(tag)), is_union(is_union),
          is_defined(is_defined)
    {
    }
    void add_field(const std::string &name, QualType type)
    {
        fields[name] = std::move(type);
    }
    std::string debug_name() const override
    {
        return (is_union ? "union " : "struct ") + tag;
    }
};

struct EnumType : public Type {
    std::string tag;
    std::unordered_map<std::string, int64_t> enumerators;
    bool is_defined;

    EnumType(std::string tag, bool is_defined = false)
        : Type(TypeKind::Enum), tag(std::move(tag)), is_defined(is_defined)
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
        : Type(TypeKind::Class), name(std::move(name)),
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
};

struct TypedefType : public Type {
    std::string name;
    QualType underlying_type;

    TypedefType(std::string name, QualType underlying_type)
        : Type(TypeKind::Typedef), name(std::move(name)),
          underlying_type(std::move(underlying_type))
    {
    }

    std::string debug_name() const override
    {
        return "typedef " + name;
    }
};

std::string type_kind_to_string(TypeKind kind);

using TypeId = size_t;

#endif // TYPE_HPP