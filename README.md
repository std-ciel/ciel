# CIEL Programming Language

[![Ubuntu Build and Test](https://github.com/std-ciel/ciel/actions/workflows/ubuntu.yaml/badge.svg?branch=main)](https://github.com/std-ciel/ciel/actions/workflows/ubuntu.yaml)

**CIEL** (*CIEL Is an Easy Language*)

## Table of Contents

- [Overview](#overview)
- [Getting Started](#getting-started)
- [Language Features](#language-features)
- [Examples](#examples)

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
- **Characters**: `char` - 8-bit character values
- **Booleans**: `bool` - Boolean values (`true` or `false`)
- **Arrays**: Homogeneous collections with compile-time bounds checking

```ciel
int count = 42;
char letter = 'A';
bool is_active = true;
bool is_complete = false;
int numbers[10] = {1, 2, 3, 4, 5};
char message[20] = "Hello, CIEL!";
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
int sum = a + b;        // Addition: 13
int diff = a - b;       // Subtraction: 7
int product = a * b;    // Multiplication: 30
int quotient = a / b;   // Division: 3
int remainder = a % b;  // Modulo: 1

// Compound assignments
a += 5;  // a is now 15
b *= 2;  // b is now 6
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
Modular programming with parameter passing and return values using CIEL's `fn` syntax:

```ciel
// Function declaration
fn calculate_factorial(int n) -> int;

// Function definition
fn calculate_factorial(int n) -> int {
    if (n <= 1) {
        return 1;
    }
    return n * calculate_factorial(n - 1);  // Recursion
}

// Function call
fn main() -> int {
    int result = calculate_factorial(5);
    printf("5! = %d\n", result);  // Output: 5! = 120
    return 0;
}
```

#### Advanced Function Features
```ciel
// Function with multiple parameters
fn max_of_three(int a, int b, int c) -> int {
    int max = (a > b) ? a : b;
    return (max > c) ? max : c;
}

// Function with pointer parameters (pass by reference)
fn swap(int* a, int* b) -> void {
    int temp = *a;
    *a = *b;
    *b = temp;
}

// Function overloading (planned feature)
fn calculate_area(int radius) -> int;                      // Circle (using integer radius)
fn calculate_area(int length, int width) -> int;           // Rectangle
fn calculate_area(int base, int height, bool) -> int;      // Triangle
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
fn main() -> int {
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
fn main() -> int {
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
fn main(int argc, char* argv[]) -> int {
    printf("Program name: %s\n", argv[0]);
    printf("Number of arguments: %d\n", argc - 1);

    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

## Examples

### Complete Program Example

```ciel
// Structure for a simple linked list node
struct Node {
    int data;
    struct Node* next;
};

// Function to create a new node
fn create_node(int value) -> struct Node* {
    struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
    new_node->data = value;
    new_node->next = NULL;
    return new_node;
}

// Function to print the linked list
fn print_list(struct Node* head) -> void {
    struct Node* current = head;
    printf("List: ");
    while (current != NULL) {
        printf("%d ", current->data);
        current = current->next;
    }
    printf("\n");
}

// Function to free the linked list
fn free_list(struct Node* head) -> void {
    struct Node* current = head;
    while (current != NULL) {
        struct Node* next = current->next;
        free(current);
        current = next;
    }
}

fn main() -> int {
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
