#ifndef TAC_UTILS_HPP
#define TAC_UTILS_HPP

#include "symbol_table/type.hpp"
#include <optional>
#include <string>

// Calculate byte offset of a member in a struct/class
std::optional<size_t> get_member_offset(const RecordType &record_type,
                                        const std::string &member_name);

// Get member type from record type
TypePtr get_member_type(const RecordType &record_type,
                        const std::string &member_name);

// Calculate size of a type (for layout calculation)
size_t calculate_type_size(TypePtr type);

// Calculate alignment requirement for a type
size_t calculate_type_alignment(TypePtr type);

#endif // TAC_UTILS_HPP
