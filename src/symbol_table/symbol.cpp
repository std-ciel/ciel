#include "symbol_table/symbol.hpp"

std::string storage_class_to_string(StorageClass storage_class)
{
    switch (storage_class) {
    case StorageClass::STATIC:
        return "static";
    case StorageClass::REGISTER:
        return "register";
    case StorageClass::AUTO:
        return "auto";
    default:
        return "unknown";
    }
}