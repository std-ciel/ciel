# CIEL Programming Language

[![Ubuntu Build and Test](https://github.com/std-ciel/ciel/actions/workflows/ubuntu.yaml/badge.svg?branch=main)](https://github.com/std-ciel/ciel/actions/workflows/ubuntu.yaml)

**CIEL** (*CIEL Is an Easy Language*)

## Table of Contents

- [Overview](#overview)
- [Getting Started](#getting-started)
- [Language Features](#language-features)
- [Examples](#examples)
- [Detailed Documentation](#detailed-documentation)

## Overview

CIEL is currently in active development and serves as a learning platform for compiler design.

## Getting Started

### Prerequisites

- CMake 3.15 or higher
- C++20 compatible compiler (GCC 10+, Clang 10+, MSVC 2019+)
- Flex (for lexical analysis)

### Quick Installation

```bash
git clone https://github.com/std-ciel/ciel.git
cd ciel
mkdir build && cd build
cmake ..
make
```

## Language Features

### Type System

CIEL provides a robust type system with both fundamental and user-defined types:

#### Fundamental Types
- **Integers**: `int` - 32-bit signed integers
- **Floating Point**: `float` - 32-bit IEEE 754 floating point numbers
- **Characters**: `char` - 8-bit character values
- **Booleans**: `bool` - Boolean values (`true` or `false`)
- **Arrays**: Homogeneous collections with compile-time bounds checking

```ciel
int count = 42;
float pi = 3.14159;
float avogadro = 6.022e23;  // Scientific notation
char letter = 'A';
bool is_active = true;
bool is_complete = false;
int numbers[10] = {1, 2, 3, 4, 5};
char message[20] = "Hello, CIEL!";
float temperatures[3] = {98.6, 37.0, 36.5};
```

#### Pointer Types
Multi-level pointer support with explicit dereferencing:

```ciel
int value = 100;
int* ptr = &value;
int** double_ptr = &ptr;

printf("Value: %d\n", *ptr);           // Output: Value: 100
printf("Double deref: %d\n", **double_ptr); // Output: Double deref: 100
```

#### User-Defined Types
Structured data types for complex data modeling:

```ciel
// Structure definition
struct Point {
    int x;
    int y;
};

// Enumeration
enum Color {
    RED,
    GREEN,
    BLUE
};

// Union for memory-efficient alternatives
union Data {
    int integer;
    char character;
};

// Type aliases for improved readability
typedef struct Point Coordinate;
typedef int Distance;
```

### Expressions and Operators

#### Arithmetic Operations
Full support for mathematical expressions with standard precedence:

```ciel
int a = 10, b = 3;
float x = 10.5, y = 3.5;
int sum = a + b;        // Addition: 13
int diff = a - b;       // Subtraction: 7
int product = a * b;    // Multiplication: 30
int quotient = a / b;   // Division: 3
int remainder = a % b;  // Modulo: 1

float fsum = x + y;     // Addition: 14.0
float fdiff = x - y;    // Subtraction: 7.0
float fprod = x * y;    // Multiplication: 36.75
float fquot = x / y;    // Division: 3.0

// Compound assignments
a += 5;  // a is now 15
b *= 2;  // b is now 6
x += 1.5; // x is now 12.0
```

#### Logical Operations
Boolean logic with short-circuit evaluation:

```ciel
int x = 5, y = 10;
bool result1 = (x > 0) && (y < 20);  // true
bool result2 = (x < 0) || (y > 5);   // true
bool result3 = !(x == y);            // true
```

### Control Flow

#### Conditional Statements
Flexible branching with if-else chains and switch statements:

```ciel
// if-else statement
if (temperature > 30) {
    printf("It's hot!\n");
} else if (temperature < 10) {
    printf("It's cold!\n");
} else {
    printf("Pleasant weather.\n");
}

// Switch statement with fall-through control
switch (day_of_week) {
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
        printf("Weekday\n");
        break;
    case 6:
    case 7:
        printf("Weekend\n");
        break;
    default:
        printf("Invalid day\n");
}
```

#### Loop Constructs
Multiple looping mechanisms for different use cases:

```ciel
// For loop with initialization, condition, and increment
for (int i = 0; i < 10; i++) {
    printf("%d ", i);
}

// While loop for condition-based iteration
int count = 0;
while (count < 5) {
    printf("Count: %d\n", count);
    count++;
}

// Do-while loop (executes at least once)
int input;
do {
    printf("Enter a positive number: ");
    scanf("%d", &input);
} while (input <= 0);

// Until loop (CIEL-specific - inverse of while)
int value = 100;
until (value == 0) {
    value--;
    if (value % 10 == 0) {
        printf("Countdown: %d\n", value);
    }
}
```

#### Jump Statements
Precise control flow management:

```ciel
// break and continue in loops
for (int i = 0; i < 20; i++) {
    if (i % 2 == 0) continue;  // Skip even numbers
    if (i > 15) break;         // Exit loop early
    printf("%d ", i);
}

// goto for specific control flow (use sparingly)
int error_code = process_data();
if (error_code != 0) goto cleanup;

// ... normal processing ...

cleanup:
    free_resources();
    return error_code;
```

### Functions

#### Function Definition and Calls
Modular programming with parameter passing and return values:

```ciel
// Function declaration
int calculate_factorial(int n);

// Function definition
int calculate_factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * calculate_factorial(n - 1);  // Recursion
}

// Function call
int main() {
    int result = calculate_factorial(5);
    printf("5! = %d\n", result);  // Output: 5! = 120
    return 0;
}
```

#### Advanced Function Features
```ciel
// Function with multiple parameters
int max_of_three(int a, int b, int c) {
    int max = (a > b) ? a : b;
    return (max > c) ? max : c;
}

// Function with pointer parameters (pass by reference)
void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Function overloading (planned feature)
int calculate_area(int radius);                      // Circle (using integer radius)
int calculate_area(int length, int width);           // Rectangle
int calculate_area(int base, int height, bool);      // Triangle
```

### Memory Management

#### Dynamic Allocation
Manual memory management with explicit control:

```ciel
#include <stdlib.h>

// Allocate memory for a single integer
int* number = (int*)malloc(sizeof(int));
*number = 42;

// Allocate memory for an array
int size = 10;
int* array = (int*)malloc(size * sizeof(int));

// Initialize array
for (int i = 0; i < size; i++) {
    array[i] = i * i;
}

// Resize array (realloc)
size = 20;
array = (int*)realloc(array, size * sizeof(int));

// Always free allocated memory
free(number);
free(array);
```

#### Static Storage Duration
Variables with program-lifetime storage:

```ciel
// Global variables
int global_counter = 0;

fn increment_counter() -> void {
    static int call_count = 0;  // Retains value between calls
    call_count++;
    global_counter += call_count;
    printf("Function called %d times\n", call_count);
}
```

### Input/Output Operations

#### Standard I/O
Formatted input and output operations:

```ciel
int main() {
    char name[50];
    int age;
    int weight;

    // Formatted output
    printf("Welcome to CIEL!\n");
    printf("Please enter your details:\n");

    // Formatted input
    printf("Name: ");
    scanf("%s", name);

    printf("Age: ");
    scanf("%d", &age);

    printf("Weight (kg): ");
    scanf("%d", &weight);

    // Display collected information
    printf("\n--- User Information ---\n");
    printf("Name: %s\n", name);
    printf("Age: %d years\n", age);
    printf("Weight: %d kg\n", weight);
    return 0;
}
```

#### File Operations
File handling for persistent data storage:

```ciel
int main() {
    FILE* file;
    char buffer[100];

    // Write to file
    file = fopen("output.txt", "w");
    if (file != NULL) {
        fprintf(file, "Hello from CIEL!\n");
        fprintf(file, "Line number: %d\n", 2);
        fclose(file);
    }

    // Read from file
    file = fopen("output.txt", "r");
    if (file != NULL) {
        while (fgets(buffer, sizeof(buffer), file) != NULL) {
            printf("Read: %s", buffer);
        }
        fclose(file);
    }

    return 0;
}
```

#### Command-Line Arguments
Program parameterization through command-line interface:

```ciel
int main(int argc, char* argv[]) {
    printf("Program name: %s\n", argv[0]);
    printf("Number of arguments: %d\n", argc - 1);

    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

## Language Quirks and Constraints

CIEL has several important constraints and behaviors that differ from C. Understanding these quirks is essential for writing valid CIEL code:

### Type System Quirks

- **No Implicit Type Conversions**: CIEL does not perform any implicit casting. All type conversions must be explicit (e.g., `(int)value`, `(float*)ptr`).
- **Exact Type Matching**: Operations like comparisons require both operands to be of the exact same type.
- **Incomplete Types**: Structs, unions, and enums must be fully defined before use (except as pointers). Forward declarations cannot be left undefined.
- **Enum Access Syntax**: Enum values use dot notation: `Color.RED` instead of just `RED`.
- **Type Keyword Required**: Must use `struct Point p;` not just `Point p;` (unless using typedef).
- **Integer-to-pointer casts restricted**: Only the integer constant 0 (null literal) may be cast to pointer types. Casting non-zero integers to pointers is not allowed and will be rejected.

### Function Quirks

- **Function Overloading**: Supported in CIEL (unlike C), based on parameter count, types, and pointer levels.
- **Named Parameters Required**: All parameters in function definitions must be named (unnamed parameters only allowed in declarations).
- **Return Statement Mandatory**: Non-void functions must contain at least one return statement with a value.

### Control Flow Quirks

- **Until Loop**: CIEL provides an `until` loop (inverse of `while`) that continues until the condition becomes true.
- **Boolean Conditions Only**: All loop and conditional statements (`if`, `while`, `for`, etc.) require boolean-typed conditions.
- **Switch Restrictions**: Switch subjects cannot be `bool` or `float` (only integral/char or enum types ). Case labels must be compile-time constants.
- **Switch-Case Restrictions**: Each case body must be enclosed within curly braces `{}` to define scope explicitly.

### Storage and Scope Quirks

- **Goto Scope**: `goto` and labels can only be used within function scope, not at global level.
- **Reserved Identifier Names**: variable names starting with `__` (double underscore) are reserved for compiler-generated identifiers and may cause name conflicts or undefined behavior. Do NOT use these.

### Object-Oriented Quirks

- **No Default Constructors**: Classes do not get automatic default constructors. You must write all constructors explicitly.
- **No Constructor/Destructor Chaining**: Parent class constructors and destructors are NOT automatically called in inheritance hierarchies.
- **No Multiple Inheritance**: Only single inheritance with multilevel chains supported (A → B → C is ok, but not A, B → C).
- **Static Members Not Supported**: No static class members or static methods.

These constraints simplify the compiler implementation while maintaining explicit control over program behavior.

## Examples

### Demonstrating Key Quirks

```ciel
// Enum access using dot notation (unlike C)
enum Color { RED, GREEN, BLUE };

// Must use 'struct' keyword when declaring (unlike C++ or with typedef)
struct Point { int x; int y; };

// Function overloading (like C++, unlike C)
int add(int a) { return a; }
int add(int a, int b) { return a + b; }

// Until loop (CIEL-specific)
int counter() {
    static int count = 0;  // static works in functions
    count = count + 1;
    return count;
}

class Example {
    private:
        int value;

    public:
        // Must write constructor explicitly (no default)
        Example(){
            this->value = 0;
        }
        Example(int v) {
            this->value = v;  // 'this' is always a pointer
        }

        // No parent constructor called automatically
        // Must handle initialization manually
};

int main() {
    enum Color c = Color.RED;  // Dot notation for enum
    struct Point p;            // 'struct' keyword required

    int x = add(5);            // Function overloading works
    int y = add(5, 10);

    int val = 5;
    // float f = val;          // ERROR: No implicit casting
    float f = (float)val;      // Must cast explicitly

    return 0;
}
```

### Complete Program Example

```ciel
// Structure for a simple linked list node
struct Node {
    int data;
    Node* next;
};

// Function to create a new node
Node* create_node(int value) {
    Node* new_node = (Node*)malloc(sizeof(Node));
    new_node->data = value;
    new_node->next = NULL;
    return new_node;
}

// Function to print the linked list
void print_list(Node* head) {
    Node* current = head;
    printf("List: ");
    while (current != NULL) {
        printf("%d ", current->data);
        current = current->next;
    }
    printf("\n");
}

// Function to free the linked list
void free_list(Node* head) {
    Node* current = head;
    while (current != NULL) {
        Node* next = current->next;
        delete current;
        current = next;
    }
}

int main() {
    // Create a simple linked list: 1 -> 2 -> 3
    Node* head = create_node(1);
    head->next = create_node(2);
    head->next->next = create_node(3);

    print_list(head);
    free_list(head);

    return 0;
}
```

## Building

### Build from Source

```bash
# Clone the repository
git clone https://github.com/std-ciel/ciel.git
cd ciel

# Create build directory
mkdir build && cd build

# Configure with CMake
cmake ..

# Build the project
make

# Run tests (if enabled)
make test
```

### Build Configuration Options

```bash
# Debug build with all warnings
cmake -DCMAKE_BUILD_TYPE=Debug ..

# Release build with optimizations
cmake -DCMAKE_BUILD_TYPE=Release ..

# Enable unit testing
cmake -DUNIT_TESTING=ON ..
```


## Detailed Documentation
- [Lexer](docs/lexer.md)
