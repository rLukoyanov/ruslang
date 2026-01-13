#!/bin/bash

# Цвета для вывода
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "  ЗАПУСК ТЕСТОВ КОМПИЛЯТОРА"
echo "=========================================="
echo ""

# Счётчики
total=0
passed=0
failed=0

# Положительные тесты (должны пройти успешно)
positive_tests=(
    "test_simple.model"
    "work.model"
    "floats.model"
    "test_arithmetic.model"
    "test_logic.model"
    "test_if_else.model"
    "test_for_loop.model"
    "test_while_loop.model"
    "test_io.model"
    "test_number_systems.model"
    "test_float_operations.model"
    "test_compound_statements.model"
    "test_complex_expressions.model"
    "test_all_operators.model"
    "test_edge_cases.model"
    "test_semantic.model"
)

echo "=== ПОЛОЖИТЕЛЬНЫЕ ТЕСТЫ ==="
echo ""

for test in "${positive_tests[@]}"; do
    total=$((total + 1))
    echo -n "Тест: $test ... "
    
    if [ -f "examples/$test" ]; then
        if go run ./ "examples/$test" > /dev/null 2>&1; then
            echo -e "${GREEN}PASSED${NC}"
            passed=$((passed + 1))
        else
            echo -e "${RED}FAILED${NC}"
            failed=$((failed + 1))
        fi
    else
        echo -e "${YELLOW}SKIPPED (файл не найден)${NC}"
    fi
done

echo ""
echo "=== ОТРИЦАТЕЛЬНЫЕ ТЕСТЫ (ДОЛЖНЫ ВЫДАТЬ ОШИБКУ) ==="
echo ""

# Отрицательные тесты (должны выдать ошибку)
negative_tests=(
    "test_error.model"
    "error_undeclared_variable.model"
    "error_redeclaration.model"
    "error_missing_semicolon.model"
    "error_invalid_number.model"
    "lexica_errors.model"
)

# Тесты с предупреждениями (не ошибками, но должны показать предупреждения)
warning_tests=(
    "error_type_mismatch.model"
)

for test in "${negative_tests[@]}"; do
    total=$((total + 1))
    echo -n "Тест: $test ... "
    
    if [ -f "examples/$test" ]; then
        if go run ./ "examples/$test" > /dev/null 2>&1; then
            echo -e "${RED}FAILED (ошибка не обнаружена)${NC}"
            failed=$((failed + 1))
        else
            echo -e "${GREEN}PASSED (ошибка обнаружена)${NC}"
            passed=$((passed + 1))
        fi
    else
        echo -e "${YELLOW}SKIPPED (файл не найден)${NC}"
    fi
done

echo ""
echo "=========================================="
echo "  РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ"
echo "=========================================="
echo -e "Всего тестов: $total"
echo -e "${GREEN}Пройдено: $passed${NC}"
echo -e "${RED}Провалено: $failed${NC}"
echo ""

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}✓ ВСЕ ТЕСТЫ ПРОЙДЕНЫ УСПЕШНО!${NC}"
    exit 0
else
    echo -e "${RED}✗ НЕКОТОРЫЕ ТЕСТЫ ПРОВАЛЕНЫ${NC}"
    exit 1
fi
