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
- Bison (for parsing)
- riscv64-gcc-linux-gnu (for RISC-V target compilation)
- qemu-riscv64 (for RISC-V emulation)

### Quick Installation

```bash
git clone https://github.com/std-ciel/ciel.git
cd ciel
mkdir build && cd build
cmake ..
make
```

## Language Features
CIEL is designed for 64-bit RISC-V architecture.

### Type System

CIEL provides a robust type system with both fundamental and user-defined types:

#### Fundamental Types

- **Integers**: `int` - 64-bit signed integers
- **Unsigned Integers**: `unsigned` - 64-bit unsigned integers
- **Floating Point**: `float` - 64-bit IEEE 754 floating point numbers
- **Characters**: `char` - 8-bit character values
- **Booleans**: `bool` - Boolean values (`true` or `false`)
- **Voids**: `void` - Represents absence of value

```cpp
int count = 42;
float pi = 3.14159;
float avogadro = 6.022e23;  // Scientific notation
char letter = 'A';
bool is_active = true;
bool is_complete = false;
```

#### Pointer Types
Multi-level pointer support with explicit dereferencing:

```cpp
int value = 100;
int* ptr = &value;
int** double_ptr = &ptr;

printf("Value: %d\n", *ptr);           // Output: Value: 100
printf("Double deref: %d\n", **double_ptr); // Output: Double deref: 100

// Null pointer requires explicit cast from 0
int* null_ptr = (int*)0;
```
#### Array Types
Fixed-size arrays with zero-based indexing. One-dimensional arrays decay to pointers when passed to functions, but multi-dimensional arrays do not:

```cpp
int numbers[10] = {1, 2, 3, 4, 5};
char message[20] = "Hello, CIEL!";
float temperatures[3] = {98.6, 37.0, 36.5};

// Multi-dimensional arrays
int matrix[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
int cube[2][2][2];  // 3D array

// sizeof works on all types
int size = sizeof(int);           // Size of int type
int arr_size = sizeof(numbers);   // Total size of array in bytes
```

#### User-Defined Types
Structured data types for complex data modeling:

```cpp
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

// Using typedef'd names - no prefix needed
Coordinate coord;           // OK: typedef name doesn't need prefix
Distance d = 100;           // OK: primitive type alias

// But using original names still requires prefix
struct Point p;             // Must use 'struct' with original name

// Class definition with single inheritance
// Note: Default access level is private
// Important: Must use 'class' keyword when declaring variables: class Shape s;
class Shape {
    public:
        void draw();
};

class Circle : Shape {
    private:
        float radius;
    public:
        // Constructor - must be defined explicitly (no default constructor)
        // Note: 'this' is always a pointer in CIEL
        Circle(float r) {
            this->radius = r;
        }
        void draw();
};

class Rectangle : Shape {
    private:
        float width;
        float height;
    public:
        Rectangle(float w, float h) {
            this->width = w;
            this->height = h;
        }
        void draw();
};

```

### Expressions and Operators

#### Arithmetic Operations
Full support for mathematical expressions with standard precedence:

```cpp
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

// Implicit upward casting for fundamental types
float result = a + x;   // int implicitly cast to float
int i = 5;
float f = i;            // OK: upward cast from int to float
// float f2 = 3.14;
// int j = f2;          // ERROR: No downward cast allowed
```

#### Logical Operations
Boolean logic with short-circuit evaluation:

```cpp
int x = 5, y = 10;
bool result1 = (x > 0) && (y < 20);  // true
bool result2 = (x < 0) || (y > 5);   // true
bool result3 = !(x == y);            // true

// Comparison between int and float is allowed
int a = 5;
float b = 5.0;
bool equal = (a == b);               // true (implicit upward cast)

// Bitwise operators
int mask = 0xFF;
int bits = 0xA5 & mask;              // Bitwise AND
int shifted = bits << 2;             // Left shift
```

### Control Flow

#### Conditional Statements
Flexible branching with if-else chains and switch statements:

```cpp
// if-else statement
if (temperature > 30) {
    printf("It's hot!\n");
} else if (temperature < 10) {
    printf("It's cold!\n");
} else {
    printf("Pleasant weather.\n");
}

// Switch statement - case bodies must be in curly braces
switch (day_of_week) {
    case 1:
    case 2:
    case 3:
    case 4:
    case 5: {
        printf("Weekday\n");
        break;
    }
    case 6:
    case 7: {
        printf("Weekend\n");
        break;
    }
    default: {
        printf("Invalid day\n");
    }
}

// Switch with enum values (using dot notation)
enum Color { RED, GREEN, BLUE };
enum Color c = Color.RED;
switch (c) {
    case Color.RED: {
        printf("Red color\n");
        break;
    }
    case Color.GREEN: {
        printf("Green color\n");
        break;
    }
    case Color.BLUE: {
        printf("Blue color\n");
        break;
    }
}
```

#### Loop Constructs
Multiple looping mechanisms for different use cases:

```cpp
// For loop with initialization, condition, and increment
for (int i = 0; i < 10; i++) {
    printf("%d ", i);
}

// Multiple variable declarations in for loop
for (int i = 0, j = 10; i < j; i++, j--) {
    printf("i=%d, j=%d\n", i, j);
}

// Note: Range-based for loops are NOT supported

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

```cpp
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

```cpp
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
```cpp
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

// Function overloading
int calculate_area(int radius);                      // Circle (using integer radius)
int calculate_area(int length, int width);           // Rectangle
int calculate_area(int base, int height, bool);      // Triangle
```

### Memory Management

#### Dynamic Allocation
Manual memory management with malloc/free

```cpp
// Note: No standard library - you must declare library functions yourself
void* malloc(unsigned size);
void free(void* ptr);
void* realloc(void* ptr, unsigned size);

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

```cpp
// Global variables
int global_counter = 0;

void increment_counter() {
    static int call_count = 0;  // Retains value between calls
    call_count++;
    global_counter += call_count;
    printf("Function called %d times\n", call_count);
}
```

### Input/Output Operations

#### Standard I/O
Formatted input and output operations:

```cpp
// CIEL has no standard library or preprocessor
// You must declare library functions you want to use
int printf(char* format, ...);
int scanf(char* format, ...);

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

```cpp
// Declare FILE type and functions needed
struct FILE;
struct FILE* fopen(char* filename, char* mode);
int fclose(struct FILE* stream);
int fprintf(struct FILE* stream, char* format, ...);
char* fgets(char* str, int n, struct FILE* stream);

int main() {
    struct FILE* file;
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

```cpp
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

- **Limited Implicit Type Conversions**: CIEL allows implicit upward casting for fundamental types (e.g., `int` to `float`), but NO implicit downward casting (e.g., `float` to `int`). No implicit casting for class types.
- **Comparisons Between Types**: Comparisons between `int` and `float` are allowed (with implicit upward cast).
- **No Null Pointer Literal**: There is no `NULL` or `nullptr`. You must explicitly cast 0 to the required pointer type: `(int*)0`.
- **Incomplete Types**: Structs, unions, and enums must be fully defined before use (except as pointers). Forward declarations cannot be left undefined.
- **Enum Access Syntax**: Enum values use dot notation: `Color.RED` instead of just `RED`.
- **Type Keyword Required**: When using the original name, must always use the full type specifier: `struct Point p;`, `enum Color c;`, `union Data d;`, `class Shape s;`. However, typedef'd names can be used without prefixes: `typedef struct Point Point_t; Point_t p;` is valid.
- **Integer-to-pointer casts restricted**: Only the integer constant 0 (null literal) may be cast to pointer types. Casting non-zero integers to pointers is not allowed and will be rejected.
- **Strict-Naming**: If a struct has been defined with a name, that name cannot be reused for a typedef or another struct/union/enum.
- **Multi-dimensional Arrays**: Do not decay to pointers when passed to functions (unlike 1D arrays).
- **Global Variables**: Global variables must be initialized with constant expressions. You cannot initialise them using non constant expressions.

### Function Quirks

- **Function Overloading**: Supported in CIEL (unlike C), based on parameter count, types, and pointer levels.
- **Named Parameters Required**: All parameters in function definitions must be named (unnamed parameters only allowed in declarations).
- **Return Statement Mandatory**: Non-void functions must contain at least one return statement with a value.

### Control Flow Quirks

- **Until Loop**: CIEL provides an `until` loop (inverse of `while`) that continues until the condition becomes true.
- **Boolean Conditions Only**: All loop and conditional statements (`if`, `while`, `for`, etc.) require boolean-typed conditions.
- **Switch Restrictions**: Switch subjects cannot be `bool` or `float` (only integral/char or enum types). Case labels must be compile-time constants.
- **Switch-Case Restrictions**: Each case body must be enclosed within curly braces `{}` to define scope explicitly.
- **Enum in Switch**: When using enum values in switch cases, use dot notation: `case Color.RED:`.
- **No Range-Based For**: Range-based for loops are not supported.
- **Multiple Variables in For**: For loops can declare multiple variables: `for (int i = 0, j = 10; ...)`.

### Storage and Scope Quirks

- **Goto Scope**: `goto` and labels can only be used within function scope, not at global level.
- **Reserved Identifier Names**: variable names starting with `__` (double underscore) are reserved for compiler-generated identifiers and may cause name conflicts or undefined behavior. Do NOT use these.

### Operators and Language Features

- **Bitwise Operators**: Supported (`&`, `|`, `^`, `<<`, `>>`, `~`).
- **Ternary Operator**: Supported (`condition ? true_value : false_value`).
- **sizeof Operator**: Supported for all types and variables.
- **No Preprocessor**: CIEL has no preprocessor. No `#include`, `#define`, `#ifdef`, etc.

### Standard Library

- **No Standard Library**: CIEL does not provide a standard library.
- **Manual Function Declarations**: You must declare the signatures of any library functions you want to use (e.g., `printf`, `malloc`, `fopen`).
- **Platform Libraries**: You can link against platform libraries but must provide the declarations yourself.

### Object-Oriented Quirks

- **No Default Constructors**: Classes do not get automatic default constructors. You must write all constructors explicitly.
- **No Constructor/Destructor Chaining**: Parent class constructors and destructors are NOT automatically called in inheritance hierarchies. You cannot manually call parent constructors.
- **No Initializer Lists**: Constructor initializer lists (`: member(value)`) are not supported. Use assignment in constructor body.
- **No Virtual Functions**: No virtual methods or polymorphism support.
- **this is Always a Pointer**: Unlike some languages, `this` is always a pointer, so use `this->member` not `this.member`.
- **Default Access is Private**: Class members are private by default. Use `public:` or `private:` access specifiers explicitly.
- **No Multiple Inheritance**: Only single inheritance with multilevel chains supported (A → B → C is ok, but not A, B → C).
- **No Static Members**: No static class members or static methods.
- **No new/delete Operators**: Use malloc/free for dynamic memory allocation even for classes.
- **Destructor Calling**: Destructors are automatically called for local objects when before every return from the defined scope and at the natural ending of the scope.

These constraints simplify the compiler implementation while maintaining explicit control over program behavior.

### Lambda Functions

CIEL supports lambda functions with explicit capture syntax:

```cpp
[capture_list] identifier (parameter_list) -> return_type {
    // function body
};
```

**Lambda Restrictions:**
- The identifier is **mandatory** and serves as the name of the lambda function
- The capture list can be empty or contain variable names to capture from the enclosing scope
- Captures must use explicit assignment syntax: `[x = x, a = y]` (capture-all `[=]` or `[&]` is NOT supported)
- Each captured variable must be explicitly named
- Capture by reference is NOT supported; all captures are by value

```cpp
int main() {
    int x = 10;
    int y = 20;

    // Lambda with explicit captures
    [add = add, x = x, y = y] add(int a, int b) -> int {
        return a + b + x + y;
    };

    int result = add(5, 3);  // result = 38
    return 0;
}
```

## Examples

### Demonstrating Key Quirks

```cpp
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
    enum Color c = Color.RED;  // Must use 'enum' prefix
    struct Point p;            // Must use 'struct' prefix
    class Example e;           // Must use 'class' prefix

    int x = add(5);            // Function overloading works
    int y = add(5, 10);

    int val = 5;
    float f = val;             // OK: Implicit upward cast (int to float)
    // int i = 3.14;           // ERROR: No implicit downward cast
    int i = (int)3.14;         // Must cast explicitly for downward conversion

    // No null pointer literal - must cast 0 explicitly
    int* ptr = (int*)0;

    return 0;
}
```

### Complete Program Example

```cpp
// Structure for a simple linked list node
struct Node {
    int data;
    struct Node* next;  // Must use 'struct' prefix even for member types
};

// Declare library functions needed
void* malloc(unsigned size);
void free(void* ptr);
int printf(char* format, ...);

// Function to create a new node
struct Node* create_node(int value) {
    struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
    new_node->data = value;
    new_node->next = (struct Node*)0;  // No NULL literal - must cast 0
    return new_node;
}

// Function to print the linked list
void print_list(struct Node* head) {
    struct Node* current = head;
    printf("List: ");
    while (current != (struct Node*)0) {  // No NULL - explicit cast required
        printf("%d ", current->data);
        current = current->next;
    }
    printf("\n");
}

// Function to free the linked list
void free_list(struct Node* head) {
    struct Node* current = head;
    while (current != (struct Node*)0) {
        struct Node* next = current->next;
        free(current);  // Use free, not delete
        current = next;
    }
}

int main() {
    // Create a simple linked list: 1 -> 2 -> 3
    struct Node* head = create_node(1);
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
