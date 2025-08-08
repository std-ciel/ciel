#include <FlexLexer.h>
#include <fstream>
#include <iostream>

int main(int argc, char *argv[])
{
    yyFlexLexer lexer;
    std::ifstream file;

    if (argc > 1) {
        file.open(argv[1]);
        if (!file.is_open()) {
            std::cerr << "Error: Could not open file " << argv[1] << std::endl;
            return 1;
        }
        lexer.switch_streams(&file, &std::cout);
    } else {
        lexer.switch_streams(&std::cin, &std::cout);
    }

    while (lexer.yylex() != 0)
        ;

    return 0;
}
