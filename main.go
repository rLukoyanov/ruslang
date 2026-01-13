package main

import (
	"fmt"
	"os"
	"strings"
)

// ANSI colors (simple helper)
const (
	colorReset  = "0"
	colorRed    = "31"
	colorGreen  = "32"
	colorYellow = "33"
	colorCyan   = "36"
)

func colorize(s, colorCode string) string {
	return fmt.Sprintf("\x1b[%sm%s\x1b[0m", colorCode, s)
}

// Типы токенов
type TokenType int

const (
	// Ключевые слова (0-16)
	TOKEN_INT TokenType = iota
	TOKEN_FLOAT
	TOKEN_BOOL
	TOKEN_IF
	TOKEN_ELSE
	TOKEN_BEGIN
	TOKEN_END
	TOKEN_FOR
	TOKEN_TO
	TOKEN_NEXT
	TOKEN_WHILE
	TOKEN_READLN
	TOKEN_WRITELN
	TOKEN_TRUE
	TOKEN_FALSE
	TOKEN_STEP
	TOKEN_VAR

	// Разделители (17-38)
	TOKEN_LPAREN
	TOKEN_RPAREN
	TOKEN_COMMA
	TOKEN_ASSIGN
	TOKEN_TILDE
	TOKEN_PERCENT
	TOKEN_LBRACE
	TOKEN_RBRACE
	TOKEN_SEMICOLON
	TOKEN_NE
	TOKEN_EQ
	TOKEN_LT
	TOKEN_LE
	TOKEN_GT
	TOKEN_GE
	TOKEN_DIV
	TOKEN_MIN
	TOKEN_PLUS
	TOKEN_MULT
	TOKEN_OR
	TOKEN_AND
	TOKEN_COLON

	// Другие токены
	TOKEN_IDENTIFIER
	TOKEN_NUMBER
	TOKEN_STRING
	TOKEN_EOF
	TOKEN_ERROR
)

// Структура токена
type Token struct {
	Type   TokenType
	Lexeme string
	Line   int
	Column int
}

// Таблица ключевых слов
var keywords = map[string]TokenType{
	"int":     TOKEN_INT,
	"float":   TOKEN_FLOAT,
	"bool":    TOKEN_BOOL,
	"if":      TOKEN_IF,
	"else":    TOKEN_ELSE,
	"begin":   TOKEN_BEGIN,
	"end":     TOKEN_END,
	"for":     TOKEN_FOR,
	"to":      TOKEN_TO,
	"next":    TOKEN_NEXT,
	"while":   TOKEN_WHILE,
	"readln":  TOKEN_READLN,
	"writeln": TOKEN_WRITELN,
	"true":    TOKEN_TRUE,
	"false":   TOKEN_FALSE,
	"step":    TOKEN_STEP,
	"var":     TOKEN_VAR,
}

// Таблица операторов (многосимвольные операторы)
var multiCharOperators = map[string]TokenType{
	"NE":   TOKEN_NE,
	"EQ":   TOKEN_EQ,
	"LT":   TOKEN_LT,
	"LE":   TOKEN_LE,
	"GT":   TOKEN_GT,
	"GE":   TOKEN_GE,
	"div":  TOKEN_DIV,
	"min":  TOKEN_MIN,
	"plus": TOKEN_PLUS,
	"mult": TOKEN_MULT,
	"or":   TOKEN_OR,
	"and":  TOKEN_AND,
}

// Лексический анализатор
type Lexer struct {
	source  string  // Исходный код
	tokens  []Token // Список распознанных токенов
	start   int     // Начало текущей лексемы
	current int     // Текущая позиция в исходном коде
	line    int     // Текущая строка
	column  int     // Текущий столбец
}

// Создание нового лексического анализатора
func NewLexer(source string) *Lexer {
	return &Lexer{
		source: source,
		line:   1,
		column: 1,
	}
}

// Основной метод сканирования токенов
func (l *Lexer) ScanTokens() []Token {
	for !l.isAtEnd() {
		l.start = l.current
		l.scanToken()
	}

	// Добавляем токен конца файла
	l.addToken(TOKEN_EOF, "")
	return l.tokens
}

// Сканирование одного токена
func (l *Lexer) scanToken() {
	c := l.advance()

	switch c {
	case ' ', '\t', '\r':
		// Пропускаем пробельные символы
	case '\n':
		// Новая строка
		l.line++
		l.column = 0
	case '"':
		l.string()
	case '(':
		l.addToken(TOKEN_LPAREN, "(")
	case ')':
		l.addToken(TOKEN_RPAREN, ")")
	case ',':
		l.addToken(TOKEN_COMMA, ",")
	case ';':
		l.addToken(TOKEN_SEMICOLON, ";")
	case ':':
		if l.match('=') {
			l.addToken(TOKEN_ASSIGN, ":=")
		} else {
			l.addToken(TOKEN_COLON, ":")
		}
	case '~':
		l.addToken(TOKEN_TILDE, "~")
	case '%':
		l.handleComment()
	case '{':
		l.addToken(TOKEN_LBRACE, "{")
	case '}':
		l.addToken(TOKEN_RBRACE, "}")
	case '<':
		l.handleLessThan()
	case '>':
		l.handleGreaterThan()
	case '=':
		l.addToken(TOKEN_EQ, "=")
	case '+':
		l.addToken(TOKEN_PLUS, "+")
	case '-':
		l.addToken(TOKEN_MIN, "-")
	case '*':
		l.addToken(TOKEN_MULT, "*")
	case '/':
		l.addToken(TOKEN_DIV, "/")
	case '.':
		// Проверяем, начинается ли число с точки (например, .25)
		if isDigit(l.peek()) {
			// Это вещественное число, начинающееся с точки
			l.current-- // Возвращаемся к точке
			l.column--  // Корректируем столбец
			l.number()
		} else {
			// Это просто точка (возможно, для других целей)
			l.error("Неподдерживаемый символ '.'")
		}
	case '&':
		if l.match('&') {
			l.addToken(TOKEN_AND, "&&")
		} else {
			l.error("Ожидается '&' для оператора AND")
		}
	case '|':
		if l.match('|') {
			l.addToken(TOKEN_OR, "||")
		} else {
			l.error("Ожидается '|' для оператора OR")
		}
	case '!':
		if l.match('=') {
			l.addToken(TOKEN_NE, "!=")
		} else {
			l.error("Неподдерживаемый оператор '!'")
		}
	default:
		// Проверяем, может ли это быть шестнадцатеричное число (начинается с буквы)
		if isDigit(c) {
			l.number()
		} else if isHexStart(c) { // ← НОВАЯ ФУНКЦИЯ!
			l.hexNumberOrIdentifier()
		} else if isAlpha(c) {
			l.identifierOrKeyword()
		} else {
			l.error(fmt.Sprintf("Неизвестный символ: %c", c))
		}
	}
}

// Новая функция для проверки начала шестнадцатеричного числа
func isHexStart(c byte) bool {
	return (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')
}

// Новый метод для обработки шестнадцатеричных чисел или идентификаторов
func (l *Lexer) hexNumberOrIdentifier() {
	// Сохраняем начальную позицию
	startPos := l.current - 1

	// Собираем все шестнадцатеричные символы
	for isHexDigit(l.peek()) {
		l.advance()
	}

	// Проверяем суффикс
	if l.peek() == 'H' || l.peek() == 'h' {
		// Это шестнадцатеричное число
		l.advance() // Пропускаем H/h

		lexeme := l.source[startPos:l.current]
		addConstant(lexeme)
		l.addToken(TOKEN_NUMBER, lexeme)
		return
	}

	// Если нет суффикса H/h, то это идентификатор
	// Возвращаемся к началу и обрабатываем как обычный идентификатор
	l.current = startPos
	l.identifierOrKeyword()
}

// Обработка строкового литерала
func (l *Lexer) string() {
	l.start = l.current

	for !l.isAtEnd() && l.peek() != '"' {
		if l.peek() == '\n' {
			l.error("Незакрытая строковая константа")
			return
		}
		if l.peek() == '\\' {
			l.advance()
			if l.isAtEnd() {
				l.error("Незакрытая строковая константа")
				return
			}
			l.advance()
		} else {
			l.advance()
		}
	}

	if l.isAtEnd() {
		l.error("Незакрытая строковая константа")
		return
	}

	lexeme := l.source[l.start:l.current]
	l.advance()
	l.addToken(TOKEN_STRING, lexeme)
}

// Обработка оператора
func (l *Lexer) handleLessThan() {
	if l.match('=') {
		l.addToken(TOKEN_LE, "<=")
	} else if l.match('>') {
		l.addToken(TOKEN_NE, "<>")
	} else {
		l.addToken(TOKEN_LT, "<")
	}
}

// Обработка оператора >
func (l *Lexer) handleGreaterThan() {
	if l.match('=') {
		l.addToken(TOKEN_GE, ">=")
	} else {
		l.addToken(TOKEN_GT, ">")
	}
}

// Обработка многострочного комментария
func (l *Lexer) handleComment() {
	l.start = l.current - 1

	for !l.isAtEnd() && l.peek() != '%' && l.peek() != '\n' {
		l.advance()
	}

	if l.match('%') {
		return
	} else if l.peek() == '\n' {
		l.error("Незакрытый многострочный комментарий")
	} else if l.isAtEnd() {
		l.error("Незакрытый многострочный комментарий в конце файла")
	}
}

// Распознавание чисел
// Распознавание чисел
func (l *Lexer) number() {
	// Собираем цифры (может быть 16-ричное число без суффикса)
	for isDigit(l.peek()) {
		l.advance()
	}

	// Проверяем суффиксы в правильном порядке
	// 1. Сначала проверяем шестнадцатеричные (H) - они могут содержать A-F
	if l.peek() == 'H' || l.peek() == 'h' {
		// Сохраняем позицию ДО суффикса
		endPos := l.current
		l.advance() // Пропускаем H/h

		// Проверяем только символы до суффикса (endPos - 1)
		for i := l.start; i < endPos; i++ {
			c := l.source[i]
			if !isHexDigit(c) {
				l.error(fmt.Sprintf("Неверный символ в шестнадцатеричном числе: %c", c))
				return
			}
		}

		lexeme := l.source[l.start:l.current]
		addConstant(lexeme)
		l.addToken(TOKEN_NUMBER, lexeme)
		return
	}

	// 2. Двоичные (B)
	if l.peek() == 'B' || l.peek() == 'b' {
		l.advance()
		for i := l.start; i < l.current-1; i++ {
			if l.source[i] != '0' && l.source[i] != '1' {
				l.error("Двоичное число может содержать только 0 и 1")
				return
			}
		}
		lexeme := l.source[l.start:l.current]
		addConstant(lexeme)
		l.addToken(TOKEN_NUMBER, lexeme)
		return
	}

	// 3. Восьмеричные (O)
	if l.peek() == 'O' || l.peek() == 'o' {
		l.advance()
		for i := l.start; i < l.current-1; i++ {
			if l.source[i] < '0' || l.source[i] > '7' {
				l.error("Восьмеричное число может содержать только цифры 0-7")
				return
			}
		}
		lexeme := l.source[l.start:l.current]
		addConstant(lexeme)
		l.addToken(TOKEN_NUMBER, lexeme)
		return
	}

	// 4. Десятичные (D) или без суффикса
	if l.peek() == 'D' || l.peek() == 'd' {
		l.advance()
		// Десятичные уже проверены - только цифры 0-9
	}
	// Без суффикса тоже десятичные

	// Проверяем на вещественное число
	if l.peek() == '.' {
		l.advance()

		for isDigit(l.peek()) {
			l.advance()
		}

		if l.peek() == 'E' || l.peek() == 'e' {
			l.advance()

			if l.peek() == '+' || l.peek() == '-' {
				l.advance()
			}

			for isDigit(l.peek()) {
				l.advance()
			}
		}
	}

	lexeme := l.source[l.start:l.current]
	addConstant(lexeme)
	l.addToken(TOKEN_NUMBER, lexeme)
}

// Распознавание идентификаторов и ключевых слов
func (l *Lexer) identifierOrKeyword() {
	for isAlphaNumeric(l.peek()) {
		l.advance()
	}

	lexeme := l.source[l.start:l.current]

	if tokenType, ok := keywords[strings.ToLower(lexeme)]; ok {
		l.addToken(tokenType, lexeme)
	} else if tokenType, ok := multiCharOperators[lexeme]; ok {
		l.addToken(tokenType, lexeme)
	} else {
		// Это идентификатор - добавляем в таблицу идентификаторов
		addIdentifier(lexeme)
		l.addToken(TOKEN_IDENTIFIER, lexeme)
	}
}

// Вспомогательные методы лексера

func (l *Lexer) advance() byte {
	if l.isAtEnd() {
		return 0
	}
	l.current++
	l.column++
	return l.source[l.current-1]
}

func (l *Lexer) peek() byte {
	if l.isAtEnd() {
		return 0
	}
	return l.source[l.current]
}

func (l *Lexer) match(expected byte) bool {
	if l.isAtEnd() || l.source[l.current] != expected {
		return false
	}
	l.current++
	l.column++
	return true
}

func (l *Lexer) isAtEnd() bool {
	return l.current >= len(l.source)
}

func (l *Lexer) addToken(tokenType TokenType, lexeme string) {
	token := Token{
		Type:   tokenType,
		Lexeme: lexeme,
		Line:   l.line,
		Column: l.column - len(lexeme),
	}
	l.tokens = append(l.tokens, token)
}

func (l *Lexer) error(message string) {
	token := Token{
		Type:   TOKEN_ERROR,
		Lexeme: message,
		Line:   l.line,
		Column: l.column,
	}
	l.tokens = append(l.tokens, token)
}

// Вспомогательные функции для проверки символов
func isDigit(c byte) bool {
	return c >= '0' && c <= '9'
}

func isAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func isAlphaNumeric(c byte) bool {
	return isAlpha(c) || isDigit(c)
}

func isHexDigit(c byte) bool {
	return isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
}

// ============================================================================
// СИНТАКСИЧЕСКИЙ АНАЛИЗАТОР
// ============================================================================

// Интегрированный синтаксический и семантический анализатор
type Parser struct {
	tokens      []Token
	current     int
	errors      []string
	warnings    []string
	depth       int // Глубина вложенности для красивого вывода
	symbolTable *SymbolTable
}

// Создание нового анализатора
func NewParser(tokens []Token) *Parser {
	return &Parser{
		tokens:      tokens,
		current:     0,
		errors:      make([]string, 0),
		warnings:    make([]string, 0),
		depth:       0,
		symbolTable: NewSymbolTable(),
	}
}

// Вспомогательные методы парсера

func (p *Parser) isAtEnd() bool {
	return p.current >= len(p.tokens) || p.tokens[p.current].Type == TOKEN_EOF
}

func (p *Parser) peek() Token {
	if p.isAtEnd() {
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.current]
}

func (p *Parser) previous() Token {
	return p.tokens[p.current-1]
}

func (p *Parser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

func (p *Parser) check(tokenType TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Type == tokenType
}

func (p *Parser) match(types ...TokenType) bool {
	for _, t := range types {
		if p.check(t) {
			p.advance()
			return true
		}
	}
	return false
}

func (p *Parser) expect(tokenType TokenType, message string) bool {
	if p.check(tokenType) {
		p.advance()
		return true
	}
	p.error(message)
	return false
}

func (p *Parser) error(message string) {
	token := p.peek()
	errorMsg := fmt.Sprintf("Строка %d, столбец %d: %s. Получен: '%s'",
		token.Line, token.Column, message, token.Lexeme)
	p.errors = append(p.errors, errorMsg)
}

func (p *Parser) semanticError(message string, token Token) {
	errorMsg := fmt.Sprintf("Строка %d, столбец %d: %s (токен: '%s')",
		token.Line, token.Column, message, token.Lexeme)
	p.errors = append(p.errors, errorMsg)
}

func (p *Parser) warning(message string, token Token) {
	warningMsg := fmt.Sprintf("Строка %d, столбец %d: %s",
		token.Line, token.Column, message)
	p.warnings = append(p.warnings, warningMsg)
}

func (p *Parser) printRule(ruleName string) {
	indent := strings.Repeat("  ", p.depth)
	fmt.Printf("%s→ %s %s\n", indent, ruleName, p.peek().Lexeme)
}

// Главный метод разбора программы
func (p *Parser) Parse() bool {
	fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorCyan))
	fmt.Println(colorize("║     СИНТАКСИЧЕСКИЙ И СЕМАНТИЧЕСКИЙ АНАЛИЗ              ║", colorCyan))
	fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorCyan))

	p.parseProgram()

	// Проверка неиспользуемых переменных
	unusedVars := p.symbolTable.GetUnusedVariables()
	for _, varName := range unusedVars {
		symbol, _ := p.symbolTable.Get(varName)
		p.warning(fmt.Sprintf("переменная '%s' объявлена, но не используется", varName),
			Token{Line: symbol.Line, Column: symbol.Column})
	}

	// Вывод таблицы символов
	p.printSymbolTable()

	// Вывод предупреждений
	if len(p.warnings) > 0 {
		fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorYellow))
		fmt.Println(colorize("║     ПРЕДУПРЕЖДЕНИЯ                                     ║", colorYellow))
		fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorYellow))
		for i, warn := range p.warnings {
			fmt.Printf("%s %d: %s\n", colorize("[Предупреждение]", colorYellow), i+1, warn)
		}
	}

	if len(p.errors) > 0 {
		fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorRed))
		fmt.Println(colorize("║     ОБНАРУЖЕНЫ ОШИБКИ                                  ║", colorRed))
		fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorRed))
		for i, err := range p.errors {
			fmt.Printf("%s %d: %s\n", colorize("[Ошибка]", colorRed), i+1, err)
		}
		return false
	}

	fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorGreen))
	fmt.Println(colorize("║     ✓ АНАЛИЗ ЗАВЕРШЕН УСПЕШНО                          ║", colorGreen))
	fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorGreen))
	return true
}

func (p *Parser) printSymbolTable() {
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     ТАБЛИЦА СИМВОЛОВ                                   ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	fmt.Printf("%-20s %-10s %-15s %-12s %s\n", "Имя", "Тип", "Позиция", "Объявлена", "Использована")
	fmt.Println(strings.Repeat("─", 60))

	if len(p.symbolTable.symbols) == 0 {
		fmt.Println("(пусто)")
	} else {
		for _, symbol := range p.symbolTable.symbols {
			defined := "✓"
			if !symbol.IsDefined {
				defined = "✗"
			}
			used := "✓"
			if !symbol.IsUsed {
				used = "✗"
			}
			pos := fmt.Sprintf("%d:%d", symbol.Line, symbol.Column)
			fmt.Printf("%-20s %-10s %-15s %-12s %s\n",
				symbol.Name, symbol.Type.String(), pos, defined, used)
		}
	}
}

// <программа>::= begin (<описание> | <оператор>) {; (<описание> | <оператор>)} end
func (p *Parser) parseProgram() {
	p.printRule("<программа>")
	p.depth++

	if !p.expect(TOKEN_BEGIN, "Ожидается 'begin'") {
		p.depth--
		return
	}

	// Проверяем, не пустая ли программа
	if p.check(TOKEN_END) {
		p.advance()
		p.depth--
		return
	}

	// Первый элемент (описание или оператор)
	if p.check(TOKEN_VAR) {
		p.parseDescription()
	} else {
		p.parseStatement()
	}

	// Последующие элементы
	// После описания точка с запятой уже есть внутри него
	// После оператора нужна точка с запятой
	for !p.check(TOKEN_END) && !p.isAtEnd() {
		// Если следующий токен - var, то это новое описание
		if p.check(TOKEN_VAR) {
			p.parseDescription()
		} else {

			if p.match(TOKEN_SEMICOLON) {
				p.parseStatement()
			}
			// Точка с запятой перед оператором
			if p.check(TOKEN_END) {
				break
			}

			p.parseStatement()
		}
	}

	if !p.expect(TOKEN_END, "Ожидается 'end'") {
		p.depth--
		return
	}

	p.depth--
}

// <описание>::= var {<идентификатор> {, <идентификатор>} : <тип> ;}
func (p *Parser) parseDescription() {
	p.printRule("<описание>")
	p.depth++

	if !p.expect(TOKEN_VAR, "Ожидается 'var'") {
		p.depth--
		return
	}

	// Повторяющаяся конструкция: <идентификатор> {, <идентификатор>} : <тип> ;
	for p.check(TOKEN_IDENTIFIER) {
		var identifiers []Token

		// Первый идентификатор
		identToken := p.advance()
		identifiers = append(identifiers, identToken)

		// Дополнительные идентификаторы через запятую
		for p.match(TOKEN_COMMA) {
			if p.check(TOKEN_IDENTIFIER) {
				identToken = p.advance()
				identifiers = append(identifiers, identToken)
			}
		}

		// Двоеточие
		if !p.expect(TOKEN_COLON, "Ожидается ':'") {
			p.depth--
			return
		}

		// Тип данных
		varType := p.parseType()

		// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: добавляем переменные в таблицу символов
		for _, ident := range identifiers {
			err := p.symbolTable.Add(ident.Lexeme, varType, ident.Line, ident.Column)
			if err != nil {
				p.semanticError(err.Error(), ident)
			}
		}

		// Точка с запятой
		if p.match(TOKEN_SEMICOLON) {
			p.depth--
			return
		}
		if !p.expect(TOKEN_SEMICOLON, "Ожидается ';' после объявления") {
			p.depth--
			return
		}
	}
	p.depth--
}

// <тип>::= int | float | bool
func (p *Parser) parseType() DataType {
	p.printRule("<тип>")
	p.depth++

	var varType DataType
	if p.match(TOKEN_INT) {
		varType = TYPE_INT
	} else if p.match(TOKEN_FLOAT) {
		varType = TYPE_FLOAT
	} else if p.match(TOKEN_BOOL) {
		varType = TYPE_BOOL
	} else {
		p.error("Ожидается тип данных (int, float, bool)")
		varType = TYPE_UNKNOWN
	}

	p.depth--
	return varType
}

// <оператор>::= <составной> | <присваивания> | <условный> | <фиксированного_цикла> | <условного_цикла> | <ввода> | <вывода>
func (p *Parser) parseStatement() {
	p.printRule("<оператор>")
	p.depth++

	if p.check(TOKEN_BEGIN) {
		p.parseCompoundStatement()
	} else if p.check(TOKEN_IDENTIFIER) {
		p.parseAssignment()
	} else if p.check(TOKEN_IF) {
		p.parseIfStatement()
	} else if p.check(TOKEN_FOR) {
		p.parseForStatement()
	} else if p.check(TOKEN_WHILE) {
		p.parseWhileStatement()
	} else if p.check(TOKEN_READLN) {
		p.parseReadStatement()
	} else if p.check(TOKEN_WRITELN) {
		p.parseWriteStatement()
	} else {
		p.error("Ожидается оператор")
	}

	if p.match(TOKEN_SEMICOLON) {
		p.depth--
		return
	}

	p.depth--
}

// <составной>::= begin <оператор> {; <оператор>} end
func (p *Parser) parseCompoundStatement() {
	p.printRule("<составной>")
	p.depth++

	if !p.expect(TOKEN_BEGIN, "Ожидается 'begin'") {
		p.depth--
		return
	}

	p.parseStatement()

	for p.match(TOKEN_SEMICOLON) {
		if p.check(TOKEN_END) {
			break
		}
		p.parseStatement()
	}

	if !p.expect(TOKEN_END, "Ожидается 'end'") {
		p.depth--
		return
	}

	p.depth--
}

// <присваивания>::= <идентификатор> := <выражение>
func (p *Parser) parseAssignment() DataType {
	p.printRule("<присваивания>")
	p.depth++

	identToken := p.peek()
	if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор") {
		p.depth--
		return TYPE_UNKNOWN
	}

	// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: проверяем, объявлена ли переменная
	symbol, exists := p.symbolTable.Get(identToken.Lexeme)
	if !exists {
		p.semanticError(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
	} else {
		p.symbolTable.MarkUsed(identToken.Lexeme)
	}

	if !p.expect(TOKEN_ASSIGN, "Ожидается ':='") {
		p.depth--
		return TYPE_UNKNOWN
	}

	exprType := p.parseExpression()

	// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: проверка типов
	if exists && symbol.Type != TYPE_UNKNOWN && exprType != TYPE_UNKNOWN {
		if symbol.Type != exprType {
			// Допускаем неявное преобразование int -> float
			if !(symbol.Type == TYPE_FLOAT && exprType == TYPE_INT) {
				p.warning(fmt.Sprintf("несоответствие типов: присваивание %s переменной типа %s",
					exprType, symbol.Type), identToken)
			}
		}
	}

	p.depth--
	if exists {
		return symbol.Type
	}
	return TYPE_UNKNOWN
}

// <условный>::= if (<выражение>) <оператор> [else <оператор>]
func (p *Parser) parseIfStatement() {
	p.printRule("<условный>")
	p.depth++

	if !p.expect(TOKEN_IF, "Ожидается 'if'") {
		p.depth--
		return
	}

	if !p.expect(TOKEN_LPAREN, "Ожидается '('") {
		p.depth--
		return
	}

	exprType := p.parseExpression()

	// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: условие должно быть bool
	if exprType != TYPE_BOOL && exprType != TYPE_UNKNOWN {
		p.warning("условие должно быть логического типа", p.peek())
	}

	if !p.expect(TOKEN_RPAREN, "Ожидается ')'") {
		p.depth--
		return
	}

	p.parseStatement()

	if p.match(TOKEN_ELSE) {
		p.parseStatement()
	}

	p.depth--
}

// <фиксированного_цикла>::= for <присваивания> to <выражение> [step <выражение>] <оператор> next
func (p *Parser) parseForStatement() {
	p.printRule("<фиксированного_цикла>")
	p.depth++

	if !p.expect(TOKEN_FOR, "Ожидается 'for'") {
		p.depth--
		return
	}

	p.parseAssignment()

	if !p.expect(TOKEN_TO, "Ожидается 'to'") {
		p.depth--
		return
	}

	p.parseExpression()

	if p.match(TOKEN_STEP) {
		p.parseExpression()
	}

	p.parseStatement()

	if !p.expect(TOKEN_NEXT, "Ожидается 'next'") {
		p.depth--
		return
	}

	p.depth--
}

// <условного_цикла>::= while (<выражение>) <оператор>
func (p *Parser) parseWhileStatement() {
	p.printRule("<условного_цикла>")
	p.depth++

	if !p.expect(TOKEN_WHILE, "Ожидается 'while'") {
		p.depth--
		return
	}

	if !p.expect(TOKEN_LPAREN, "Ожидается '('") {
		p.depth--
		return
	}

	exprType := p.parseExpression()

	// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: условие должно быть bool
	if exprType != TYPE_BOOL && exprType != TYPE_UNKNOWN {
		p.warning("условие цикла должно быть логического типа", p.peek())
	}

	if !p.expect(TOKEN_RPAREN, "Ожидается ')'") {
		p.depth--
		return
	}

	p.parseStatement()

	p.depth--
}

// <ввода>::= readln <идентификатор> {, <идентификатор>}
func (p *Parser) parseReadStatement() {
	p.printRule("<ввода>")
	p.depth++

	if !p.expect(TOKEN_READLN, "Ожидается 'readln'") {
		p.depth--
		return
	}

	identToken := p.peek()
	if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор") {
		p.depth--
		return
	}

	// СЕМАНТИЧЕСКАЯ ПРОВЕРКА
	if _, exists := p.symbolTable.Get(identToken.Lexeme); !exists {
		p.semanticError(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
	} else {
		p.symbolTable.MarkUsed(identToken.Lexeme)
	}

	for p.match(TOKEN_COMMA) {
		identToken = p.peek()
		if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор после ','") {
			p.depth--
			return
		}

		// СЕМАНТИЧЕСКАЯ ПРОВЕРКА
		if _, exists := p.symbolTable.Get(identToken.Lexeme); !exists {
			p.semanticError(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
		} else {
			p.symbolTable.MarkUsed(identToken.Lexeme)
		}
	}

	p.depth--
}

// <вывода>::= writeln <выражение> {, <выражение>}
func (p *Parser) parseWriteStatement() {
	p.printRule("<вывода>")
	p.depth++

	if !p.expect(TOKEN_WRITELN, "Ожидается 'writeln'") {
		p.depth--
		return
	}

	p.parseExpression()

	for p.match(TOKEN_COMMA) {
		p.parseExpression()
	}

	p.depth--
}

// <выражение>::= <операнд> {<операции_группы_отношения> <операнд>}
func (p *Parser) parseExpression() DataType {
	p.printRule("<выражение>")
	p.depth++

	leftType := p.parseOperand()

	for p.match(TOKEN_NE, TOKEN_EQ, TOKEN_LT, TOKEN_LE, TOKEN_GT, TOKEN_GE) {
		rightType := p.parseOperand()

		// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: операции сравнения возвращают bool
		if leftType != TYPE_UNKNOWN && rightType != TYPE_UNKNOWN {
			if leftType != rightType {
				if !((leftType == TYPE_INT && rightType == TYPE_FLOAT) ||
					(leftType == TYPE_FLOAT && rightType == TYPE_INT)) {
					p.warning(fmt.Sprintf("сравнение значений разных типов: %s и %s",
						leftType, rightType), p.peek())
				}
			}
		}
		leftType = TYPE_BOOL
	}

	p.depth--
	return leftType
}

// <операнд>::= <слагаемое> {<операции_группы_сложения> <слагаемое>}
func (p *Parser) parseOperand() DataType {
	p.printRule("<операнд>")
	p.depth++

	leftType := p.parseTerm()

	for p.match(TOKEN_PLUS, TOKEN_MIN) {
		rightType := p.parseTerm()

		// СЕМАНТИЧЕСКАЯ ПРОВЕРКА: арифметические операции
		if leftType == TYPE_BOOL || rightType == TYPE_BOOL {
			p.warning("арифметические операции с логическим типом", p.peek())
		}

		if leftType == TYPE_FLOAT || rightType == TYPE_FLOAT {
			leftType = TYPE_FLOAT
		}
	}

	// Логическое ИЛИ
	for p.match(TOKEN_OR) {
		rightType := p.parseTerm()

		if leftType != TYPE_BOOL || rightType != TYPE_BOOL {
			p.warning("логическая операция 'or' требует операнды типа bool", p.peek())
		}
		leftType = TYPE_BOOL
	}

	p.depth--
	return leftType
}

// <слагаемое>::= <множитель> {<операции_группы_умножения> <множитель>}
func (p *Parser) parseTerm() DataType {
	p.printRule("<слагаемое>")
	p.depth++

	leftType := p.parseFactor()

	for p.match(TOKEN_MULT, TOKEN_DIV) {
		rightType := p.parseFactor()

		// СЕМАНТИЧЕСКАЯ ПРОВЕРКА
		if leftType == TYPE_BOOL || rightType == TYPE_BOOL {
			p.warning("арифметические операции с логическим типом", p.peek())
		}

		if leftType == TYPE_FLOAT || rightType == TYPE_FLOAT {
			leftType = TYPE_FLOAT
		}
	}

	// Логическое И
	for p.match(TOKEN_AND) {
		rightType := p.parseFactor()

		if leftType != TYPE_BOOL || rightType != TYPE_BOOL {
			p.warning("логическая операция 'and' требует операнды типа bool", p.peek())
		}
		leftType = TYPE_BOOL
	}

	p.depth--
	return leftType
}

// <множитель>::= <идентификатор> | <число> | <логическая_константа> |
//
//	<унарная_операция> <множитель> | (<выражение>)
func (p *Parser) parseFactor() DataType {
	p.printRule("<множитель>")
	p.depth++

	// Унарная операция
	if p.match(TOKEN_TILDE) {
		factorType := p.parseFactor()
		if factorType != TYPE_BOOL && factorType != TYPE_UNKNOWN {
			p.warning("унарная операция '~' применима только к логическому типу", p.peek())
		}
		p.depth--
		return TYPE_BOOL
	}

	// Логические константы
	if p.match(TOKEN_TRUE, TOKEN_FALSE) {
		p.depth--
		return TYPE_BOOL
	}

	// Числа
	if p.match(TOKEN_NUMBER) {
		// Определяем тип числа
		lexeme := p.tokens[p.current-1].Lexeme
		if strings.Contains(lexeme, ".") || strings.Contains(lexeme, "E") || strings.Contains(lexeme, "e") {
			p.depth--
			return TYPE_FLOAT
		}
		p.depth--
		return TYPE_INT
	}

	// Идентификаторы
	if p.check(TOKEN_IDENTIFIER) {
		identToken := p.advance()
		symbol, exists := p.symbolTable.Get(identToken.Lexeme)
		if !exists {
			p.semanticError(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
			p.depth--
			return TYPE_UNKNOWN
		}
		p.symbolTable.MarkUsed(identToken.Lexeme)
		p.depth--
		return symbol.Type
	}

	// Выражение в скобках
	if p.match(TOKEN_LPAREN) {
		exprType := p.parseExpression()
		if !p.expect(TOKEN_RPAREN, "Ожидается ')' после выражения") {
			p.depth--
			return TYPE_UNKNOWN
		}
		p.depth--
		return exprType
	}

	p.error("Ожидается идентификатор, число, логическая константа или '('")
	p.depth--
	return TYPE_UNKNOWN
}

// ============================================================================
// ФУНКЦИИ ВЫВОДА
// ============================================================================

func PrintTokens(tokens []Token) {
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     РЕЗУЛЬТАТ ЛЕКСИЧЕСКОГО АНАЛИЗА                     ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	fmt.Printf("%-20s %-20s %-10s %s\n", "Тип токена", "Лексема", "Строка", "Столбец")
	fmt.Println(strings.Repeat("─", 65))

	for _, token := range tokens {
		if token.Type == TOKEN_EOF {
			fmt.Printf("%-20s %-20s %-10d %d\n",
				"EOF", "", token.Line, token.Column)
			break
		}

		typeName := tokenTypeToString(token.Type)
		lexeme := token.Lexeme
		if len(lexeme) > 18 {
			lexeme = lexeme[:15] + "..."
		}

		fmt.Printf("%-20s %-20s %-10d %d\n",
			typeName, lexeme, token.Line, token.Column)
	}
}

func tokenTypeToString(t TokenType) string {
	switch t {
	case TOKEN_INT:
		return "INT"
	case TOKEN_FLOAT:
		return "FLOAT"
	case TOKEN_BOOL:
		return "BOOL"
	case TOKEN_IF:
		return "IF"
	case TOKEN_ELSE:
		return "ELSE"
	case TOKEN_BEGIN:
		return "BEGIN"
	case TOKEN_END:
		return "END"
	case TOKEN_FOR:
		return "FOR"
	case TOKEN_TO:
		return "TO"
	case TOKEN_NEXT:
		return "NEXT"
	case TOKEN_WHILE:
		return "WHILE"
	case TOKEN_READLN:
		return "READLN"
	case TOKEN_WRITELN:
		return "WRITELN"
	case TOKEN_TRUE:
		return "TRUE"
	case TOKEN_FALSE:
		return "FALSE"
	case TOKEN_STEP:
		return "STEP"
	case TOKEN_VAR:
		return "VAR"
	case TOKEN_LPAREN:
		return "LPAREN"
	case TOKEN_RPAREN:
		return "RPAREN"
	case TOKEN_COMMA:
		return "COMMA"
	case TOKEN_ASSIGN:
		return "ASSIGN"
	case TOKEN_TILDE:
		return "TILDE"
	case TOKEN_PERCENT:
		return "PERCENT"
	case TOKEN_LBRACE:
		return "LBRACE"
	case TOKEN_RBRACE:
		return "RBRACE"
	case TOKEN_SEMICOLON:
		return "SEMICOLON"
	case TOKEN_NE:
		return "NE"
	case TOKEN_EQ:
		return "EQ"
	case TOKEN_LT:
		return "LT"
	case TOKEN_LE:
		return "LE"
	case TOKEN_GT:
		return "GT"
	case TOKEN_GE:
		return "GE"
	case TOKEN_DIV:
		return "DIV"
	case TOKEN_MIN:
		return "MIN"
	case TOKEN_PLUS:
		return "PLUS"
	case TOKEN_MULT:
		return "MULT"
	case TOKEN_OR:
		return "OR"
	case TOKEN_AND:
		return "AND"
	case TOKEN_COLON:
		return "COLON"
	case TOKEN_IDENTIFIER:
		return "IDENTIFIER"
	case TOKEN_NUMBER:
		return "NUMBER"
	case TOKEN_STRING:
		return "STRING"
	case TOKEN_ERROR:
		return "ERROR"
	case TOKEN_EOF:
		return "EOF"
	default:
		return "UNKNOWN"
	}
}

func createTokenTables() ([]TokenType, []TokenType) {
	keywordTable := []TokenType{
		TOKEN_INT, TOKEN_FLOAT, TOKEN_BOOL, TOKEN_IF, TOKEN_ELSE,
		TOKEN_BEGIN, TOKEN_END, TOKEN_FOR, TOKEN_TO, TOKEN_NEXT,
		TOKEN_WHILE, TOKEN_READLN, TOKEN_WRITELN, TOKEN_TRUE, TOKEN_FALSE,
		TOKEN_STEP, TOKEN_VAR,
	}

	separatorTable := []TokenType{
		TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_COMMA, TOKEN_ASSIGN, TOKEN_TILDE,
		TOKEN_PERCENT, TOKEN_LBRACE, TOKEN_RBRACE, TOKEN_SEMICOLON, TOKEN_NE,
		TOKEN_EQ, TOKEN_LT, TOKEN_LE, TOKEN_GT, TOKEN_GE,
		TOKEN_DIV, TOKEN_MIN, TOKEN_PLUS, TOKEN_MULT, TOKEN_OR,
		TOKEN_AND, TOKEN_COLON,
	}

	return keywordTable, separatorTable
}

func getTokenTableIndex(token Token, keywordTable, separatorTable []TokenType) (int, int) {
	// Проверяем в таблице ключевых слов (таблица 0)
	for i, tt := range keywordTable {
		if tt == token.Type {
			return 0, i
		}
	}

	// Проверяем в таблице разделителей (таблица 1)
	for i, tt := range separatorTable {
		if tt == token.Type {
			return 1, i
		}
	}

	// Идентификаторы (таблица 2)
	if token.Type == TOKEN_IDENTIFIER {
		for i, id := range identifierTable {
			if id == token.Lexeme {
				return 2, i
			}
		}
	}

	// Константы (таблица 3)
	if token.Type == TOKEN_NUMBER || token.Type == TOKEN_STRING {
		for i, const_val := range constantTable {
			if const_val == token.Lexeme {
				return 3, i
			}
		}
	}

	return -1, -1
}

func PrintTokensAsTableIndex(tokens []Token) {
	keywordTable, separatorTable := createTokenTables()
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     ТОКЕНЫ В ФОРМАТЕ [ТАБЛИЦА, ИНДЕКС]                 ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	fmt.Printf("%-20s %-20s %-15s\n", "Тип токена", "Лексема", "Формат")
	fmt.Println(strings.Repeat("─", 60))

	for _, token := range tokens {
		if token.Type == TOKEN_EOF {
			break
		}

		if token.Type == TOKEN_ERROR {
			continue
		}

		typeName := tokenTypeToString(token.Type)
		lexeme := token.Lexeme
		if len(lexeme) > 18 {
			lexeme = lexeme[:15] + "..."
		}

		table, index := getTokenTableIndex(token, keywordTable, separatorTable)

		if table >= 0 {
			fmt.Printf("%-20s %-20s [%d, %d]\n", typeName, lexeme, table, index)
		} else {
			fmt.Printf("%-20s %-20s %s\n", typeName, lexeme, "-")
		}
	}

}

// Объединённый вывод токенов: лексема + индекс таблицы при наличии
func PrintTokensCombined(tokens []Token) {
	keywordTable, separatorTable := createTokenTables()
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     РЕЗУЛЬТАТ ЛЕКСИЧЕСКОГО АНАЛИЗА (COMBINED)           ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	fmt.Printf("%-20s %-20s %-8s %-8s %-15s\n", "Тип токена", "Лексема", "Строка", "Столбец", "Формат")
	fmt.Println(strings.Repeat("─", 80))

	for _, token := range tokens {
		if token.Type == TOKEN_EOF {
			fmt.Printf("%-20s %-20s %-8d %-8d %-15s\n",
				"EOF", "", token.Line, token.Column, "-")
			break
		}

		if token.Type == TOKEN_ERROR {
			continue
		}

		typeName := tokenTypeToString(token.Type)
		lexeme := token.Lexeme
		if len(lexeme) > 18 {
			lexeme = lexeme[:15] + "..."
		}

		table, index := getTokenTableIndex(token, keywordTable, separatorTable)
		format := "-"
		if table >= 0 {
			format = fmt.Sprintf("[%d, %d]", table, index)
		}

		fmt.Printf("%-20s %-20s %-8d %-8d %-15s\n",
			typeName, lexeme, token.Line, token.Column, format)
	}
}

// Таблицы для идентификаторов и констант
var (
	identifierTable = make([]string, 0) // Таблица 2: Идентификаторы
	constantTable   = make([]string, 0) // Таблица 3: Константы (числа)
)

// Добавление идентификатора в таблицу 2
func addIdentifier(name string) int {
	// Проверяем, есть ли уже такой идентификатор
	for i, id := range identifierTable {
		if id == name {
			return i
		}
	}
	// Добавляем новый идентификатор
	identifierTable = append(identifierTable, name)
	return len(identifierTable) - 1
}

// Добавление константы в таблицу 3
func addConstant(value string) int {
	// Проверяем, есть ли уже такая константа
	for i, const_val := range constantTable {
		if const_val == value {
			return i
		}
	}
	// Добавляем новую константу
	constantTable = append(constantTable, value)
	return len(constantTable) - 1
}

// ============================================================================
// СЕМАНТИЧЕСКИЙ АНАЛИЗАТОР
// ============================================================================

// Типы данных
type DataType int

const (
	TYPE_UNKNOWN DataType = iota
	TYPE_INT
	TYPE_FLOAT
	TYPE_BOOL
)

func (dt DataType) String() string {
	switch dt {
	case TYPE_INT:
		return "int"
	case TYPE_FLOAT:
		return "float"
	case TYPE_BOOL:
		return "bool"
	default:
		return "unknown"
	}
}

// Символ в таблице символов
type Symbol struct {
	Name      string
	Type      DataType
	Line      int
	Column    int
	IsDefined bool
	IsUsed    bool
}

// Таблица символов
type SymbolTable struct {
	symbols map[string]*Symbol
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		symbols: make(map[string]*Symbol),
	}
}

func (st *SymbolTable) Add(name string, varType DataType, line, column int) error {
	if _, exists := st.symbols[name]; exists {
		return fmt.Errorf("переменная '%s' уже объявлена", name)
	}
	st.symbols[name] = &Symbol{
		Name:      name,
		Type:      varType,
		Line:      line,
		Column:    column,
		IsDefined: true,
		IsUsed:    false,
	}
	return nil
}

func (st *SymbolTable) Get(name string) (*Symbol, bool) {
	symbol, exists := st.symbols[name]
	return symbol, exists
}

func (st *SymbolTable) MarkUsed(name string) {
	if symbol, exists := st.symbols[name]; exists {
		symbol.IsUsed = true
	}
}

func (st *SymbolTable) GetUnusedVariables() []string {
	var unused []string
	for name, symbol := range st.symbols {
		if !symbol.IsUsed {
			unused = append(unused, name)
		}
	}
	return unused
}

// Семантический анализатор
type SemanticAnalyzer struct {
	tokens      []Token
	current     int
	symbolTable *SymbolTable
	errors      []string
	warnings    []string
	currentType DataType
}

func NewSemanticAnalyzer(tokens []Token) *SemanticAnalyzer {
	return &SemanticAnalyzer{
		tokens:      tokens,
		current:     0,
		symbolTable: NewSymbolTable(),
		errors:      make([]string, 0),
		warnings:    make([]string, 0),
	}
}

// Вспомогательные методы

func (sa *SemanticAnalyzer) isAtEnd() bool {
	return sa.current >= len(sa.tokens) || sa.tokens[sa.current].Type == TOKEN_EOF
}

func (sa *SemanticAnalyzer) peek() Token {
	if sa.isAtEnd() {
		return sa.tokens[len(sa.tokens)-1]
	}
	return sa.tokens[sa.current]
}

func (sa *SemanticAnalyzer) advance() Token {
	if !sa.isAtEnd() {
		sa.current++
	}
	return sa.tokens[sa.current-1]
}

func (sa *SemanticAnalyzer) check(tokenType TokenType) bool {
	if sa.isAtEnd() {
		return false
	}
	return sa.peek().Type == tokenType
}

func (sa *SemanticAnalyzer) match(types ...TokenType) bool {
	for _, t := range types {
		if sa.check(t) {
			sa.advance()
			return true
		}
	}
	return false
}

func (sa *SemanticAnalyzer) error(message string, token Token) {
	errorMsg := fmt.Sprintf("Строка %d, столбец %d: %s (токен: '%s')",
		token.Line, token.Column, message, token.Lexeme)
	sa.errors = append(sa.errors, errorMsg)
}

func (sa *SemanticAnalyzer) warning(message string, token Token) {
	warningMsg := fmt.Sprintf("Строка %d, столбец %d: %s",
		token.Line, token.Column, message)
	sa.warnings = append(sa.warnings, warningMsg)
}

// Главный метод анализа
func (sa *SemanticAnalyzer) Analyze() bool {
	fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorCyan))
	fmt.Println(colorize("║     СЕМАНТИЧЕСКИЙ АНАЛИЗ ПРОГРАММЫ                     ║", colorCyan))
	fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorCyan))

	sa.analyzeProgram()

	// Проверка неиспользуемых переменных
	unusedVars := sa.symbolTable.GetUnusedVariables()
	for _, varName := range unusedVars {
		symbol, _ := sa.symbolTable.Get(varName)
		sa.warning(fmt.Sprintf("переменная '%s' объявлена, но не используется", varName),
			Token{Line: symbol.Line, Column: symbol.Column})
	}

	// Вывод таблицы символов
	sa.printSymbolTable()

	// Вывод предупреждений
	if len(sa.warnings) > 0 {
		fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorYellow))
		fmt.Println(colorize("║     ПРЕДУПРЕЖДЕНИЯ                                     ║", colorYellow))
		fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorYellow))
		for i, warn := range sa.warnings {
			fmt.Printf("%s %d: %s\n", colorize("[Предупреждение]", colorYellow), i+1, warn)
		}
	}

	// Вывод ошибок
	if len(sa.errors) > 0 {
		fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorRed))
		fmt.Println(colorize("║     ОБНАРУЖЕНЫ СЕМАНТИЧЕСКИЕ ОШИБКИ                    ║", colorRed))
		fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorRed))
		for i, err := range sa.errors {
			fmt.Printf("%s %d: %s\n", colorize("[Ошибка]", colorRed), i+1, err)
		}
		return false
	}

	fmt.Println("\n" + colorize("╔════════════════════════════════════════════════════════╗", colorGreen))
	fmt.Println(colorize("║     ✓ СЕМАНТИЧЕСКИЙ АНАЛИЗ ЗАВЕРШЕН УСПЕШНО            ║", colorGreen))
	fmt.Println(colorize("╚════════════════════════════════════════════════════════╝", colorGreen))
	return true
}

func (sa *SemanticAnalyzer) printSymbolTable() {
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     ТАБЛИЦА СИМВОЛОВ                                   ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	fmt.Printf("%-20s %-10s %-15s %-12s %s\n", "Имя", "Тип", "Позиция", "Объявлена", "Использована")
	fmt.Println(strings.Repeat("─", 60))

	if len(sa.symbolTable.symbols) == 0 {
		fmt.Println("(пусто)")
	} else {
		for _, symbol := range sa.symbolTable.symbols {
			defined := "✓"
			if !symbol.IsDefined {
				defined = "✗"
			}
			used := "✓"
			if !symbol.IsUsed {
				used = "✗"
			}
			pos := fmt.Sprintf("%d:%d", symbol.Line, symbol.Column)
			fmt.Printf("%-20s %-10s %-15s %-12s %s\n",
				symbol.Name, symbol.Type.String(), pos, defined, used)
		}
	}
}

// <программа>
func (sa *SemanticAnalyzer) analyzeProgram() {
	if !sa.match(TOKEN_BEGIN) {
		return
	}

	if sa.check(TOKEN_END) {
		sa.advance()
		return
	}

	// Обрабатываем последовательность описаний и операторов
	for !sa.check(TOKEN_END) && !sa.isAtEnd() {
		if sa.check(TOKEN_VAR) {
			sa.analyzeDescription()
		} else if sa.check(TOKEN_SEMICOLON) {
			// Пропускаем лишние точки с запятой
			sa.advance()
		} else if !sa.check(TOKEN_END) {
			sa.analyzeStatement()
			// После оператора ожидаем точку с запятой (если это не конец программы)
			if !sa.check(TOKEN_END) && !sa.check(TOKEN_VAR) {
				if !sa.match(TOKEN_SEMICOLON) {
					// Точка с запятой отсутствует, но продолжаем
				}
			}
		}
	}

	sa.match(TOKEN_END)
}

// <описание>
func (sa *SemanticAnalyzer) analyzeDescription() {
	if !sa.match(TOKEN_VAR) {
		return
	}

	// После var ДОЛЖНА быть хотя бы одна конструкция идентификатор : тип ;
	// Проверяем, что следующий токен - идентификатор
	if !sa.check(TOKEN_IDENTIFIER) {
		return
	}

	// Обрабатываем одну группу объявлений: <идентификатор> {, <идентификатор>} : <тип> ;
	var identifiers []Token

	// Собираем идентификаторы
	identToken := sa.advance()
	identifiers = append(identifiers, identToken)

	for sa.match(TOKEN_COMMA) {
		if sa.check(TOKEN_IDENTIFIER) {
			identToken = sa.advance()
			identifiers = append(identifiers, identToken)
		}
	}

	if !sa.match(TOKEN_COLON) {
		return
	}

	// Получаем тип
	varType := sa.analyzeType()

	// Добавляем переменные в таблицу символов
	for _, ident := range identifiers {
		err := sa.symbolTable.Add(ident.Lexeme, varType, ident.Line, ident.Column)
		if err != nil {
			sa.error(err.Error(), ident)
		}
	}

	if !sa.match(TOKEN_SEMICOLON) {
		return
	}
}

// <тип>
func (sa *SemanticAnalyzer) analyzeType() DataType {
	if sa.match(TOKEN_INT) {
		return TYPE_INT
	} else if sa.match(TOKEN_FLOAT) {
		return TYPE_FLOAT
	} else if sa.match(TOKEN_BOOL) {
		return TYPE_BOOL
	}
	return TYPE_UNKNOWN
}

// <оператор>
func (sa *SemanticAnalyzer) analyzeStatement() {
	if sa.check(TOKEN_BEGIN) {
		sa.analyzeCompoundStatement()
	} else if sa.check(TOKEN_IDENTIFIER) {
		sa.analyzeAssignment()
	} else if sa.check(TOKEN_IF) {
		sa.analyzeIfStatement()
	} else if sa.check(TOKEN_FOR) {
		sa.analyzeForStatement()
	} else if sa.check(TOKEN_WHILE) {
		sa.analyzeWhileStatement()
	} else if sa.check(TOKEN_READLN) {
		sa.analyzeReadStatement()
	} else if sa.check(TOKEN_WRITELN) {
		sa.analyzeWriteStatement()
	}
	// Не потребляем точку с запятой здесь - она обрабатывается в analyzeProgram
}

// <составной>
func (sa *SemanticAnalyzer) analyzeCompoundStatement() {
	if !sa.match(TOKEN_BEGIN) {
		return
	}

	sa.analyzeStatement()

	for sa.match(TOKEN_SEMICOLON) {
		if sa.check(TOKEN_END) {
			break
		}
		sa.analyzeStatement()
	}

	sa.match(TOKEN_END)
}

// <присваивания>
func (sa *SemanticAnalyzer) analyzeAssignment() {
	if !sa.check(TOKEN_IDENTIFIER) {
		return
	}

	identToken := sa.advance()

	// Проверяем, объявлена ли переменная
	symbol, exists := sa.symbolTable.Get(identToken.Lexeme)
	if !exists {
		sa.error(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
	} else {
		sa.symbolTable.MarkUsed(identToken.Lexeme)
	}

	if !sa.match(TOKEN_ASSIGN) {
		return
	}

	// Анализируем выражение справа
	exprType := sa.analyzeExpression()

	// Проверка типов
	if exists && symbol.Type != TYPE_UNKNOWN && exprType != TYPE_UNKNOWN {
		if symbol.Type != exprType {
			// Допускаем неявное преобразование int -> float
			if !(symbol.Type == TYPE_FLOAT && exprType == TYPE_INT) {
				sa.warning(fmt.Sprintf("несоответствие типов: присваивание %s переменной типа %s",
					exprType, symbol.Type), identToken)
			}
		}
	}
}

// <условный>
func (sa *SemanticAnalyzer) analyzeIfStatement() {
	if !sa.match(TOKEN_IF) {
		return
	}

	if !sa.match(TOKEN_LPAREN) {
		return
	}

	exprType := sa.analyzeExpression()

	// Проверяем, что условие логического типа
	if exprType != TYPE_BOOL && exprType != TYPE_UNKNOWN {
		sa.warning("условие должно быть логического типа", sa.peek())
	}

	if !sa.match(TOKEN_RPAREN) {
		return
	}

	sa.analyzeStatement()

	if sa.match(TOKEN_ELSE) {
		sa.analyzeStatement()
	}
}

// <фиксированного_цикла>
func (sa *SemanticAnalyzer) analyzeForStatement() {
	if !sa.match(TOKEN_FOR) {
		return
	}

	sa.analyzeAssignment()

	if !sa.match(TOKEN_TO) {
		return
	}

	sa.analyzeExpression()

	if sa.match(TOKEN_STEP) {
		sa.analyzeExpression()
	}

	sa.analyzeStatement()

	sa.match(TOKEN_NEXT)
}

// <условного_цикла>
func (sa *SemanticAnalyzer) analyzeWhileStatement() {
	if !sa.match(TOKEN_WHILE) {
		return
	}

	if !sa.match(TOKEN_LPAREN) {
		return
	}

	exprType := sa.analyzeExpression()

	// Проверяем, что условие логического типа
	if exprType != TYPE_BOOL && exprType != TYPE_UNKNOWN {
		sa.warning("условие цикла должно быть логического типа", sa.peek())
	}

	if !sa.match(TOKEN_RPAREN) {
		return
	}

	sa.analyzeStatement()
}

// <ввода>
func (sa *SemanticAnalyzer) analyzeReadStatement() {
	if !sa.match(TOKEN_READLN) {
		return
	}

	if !sa.check(TOKEN_IDENTIFIER) {
		return
	}

	identToken := sa.advance()

	// Проверяем объявление
	if _, exists := sa.symbolTable.Get(identToken.Lexeme); !exists {
		sa.error(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
	} else {
		sa.symbolTable.MarkUsed(identToken.Lexeme)
	}

	for sa.match(TOKEN_COMMA) {
		if sa.check(TOKEN_IDENTIFIER) {
			identToken = sa.advance()
			if _, exists := sa.symbolTable.Get(identToken.Lexeme); !exists {
				sa.error(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
			} else {
				sa.symbolTable.MarkUsed(identToken.Lexeme)
			}
		}
	}
}

// <вывода>
func (sa *SemanticAnalyzer) analyzeWriteStatement() {
	if !sa.match(TOKEN_WRITELN) {
		return
	}

	sa.analyzeExpression()

	for sa.match(TOKEN_COMMA) {
		sa.analyzeExpression()
	}
}

// <выражение>
func (sa *SemanticAnalyzer) analyzeExpression() DataType {
	leftType := sa.analyzeOperand()

	for sa.match(TOKEN_NE, TOKEN_EQ, TOKEN_LT, TOKEN_LE, TOKEN_GT, TOKEN_GE) {
		rightType := sa.analyzeOperand()

		// Операции сравнения возвращают bool
		if leftType != TYPE_UNKNOWN && rightType != TYPE_UNKNOWN {
			if leftType != rightType {
				// Допускаем сравнение int с float
				if !((leftType == TYPE_INT && rightType == TYPE_FLOAT) ||
					(leftType == TYPE_FLOAT && rightType == TYPE_INT)) {
					sa.warning(fmt.Sprintf("сравнение значений разных типов: %s и %s",
						leftType, rightType), sa.peek())
				}
			}
		}
		leftType = TYPE_BOOL
	}

	return leftType
}

// <операнд>
func (sa *SemanticAnalyzer) analyzeOperand() DataType {
	leftType := sa.analyzeTerm()

	for sa.match(TOKEN_PLUS, TOKEN_MIN) {
		rightType := sa.analyzeTerm()

		// Арифметические операции
		if leftType == TYPE_BOOL || rightType == TYPE_BOOL {
			sa.warning("арифметические операции с логическим типом", sa.peek())
		}

		// Результат: если хоть один float, то float
		if leftType == TYPE_FLOAT || rightType == TYPE_FLOAT {
			leftType = TYPE_FLOAT
		}
	}

	// Логическое ИЛИ
	for sa.match(TOKEN_OR) {
		rightType := sa.analyzeTerm()

		if leftType != TYPE_BOOL || rightType != TYPE_BOOL {
			sa.warning("логическая операция 'or' требует операнды типа bool", sa.peek())
		}
		leftType = TYPE_BOOL
	}

	return leftType
}

// <слагаемое>
func (sa *SemanticAnalyzer) analyzeTerm() DataType {
	leftType := sa.analyzeFactor()

	for sa.match(TOKEN_MULT, TOKEN_DIV) {
		rightType := sa.analyzeFactor()

		// Арифметические операции
		if leftType == TYPE_BOOL || rightType == TYPE_BOOL {
			sa.warning("арифметические операции с логическим типом", sa.peek())
		}

		// Результат: если хоть один float, то float
		if leftType == TYPE_FLOAT || rightType == TYPE_FLOAT {
			leftType = TYPE_FLOAT
		}
	}

	// Логическое И
	for sa.match(TOKEN_AND) {
		rightType := sa.analyzeFactor()

		if leftType != TYPE_BOOL || rightType != TYPE_BOOL {
			sa.warning("логическая операция 'and' требует операнды типа bool", sa.peek())
		}
		leftType = TYPE_BOOL
	}

	return leftType
}

// <множитель>
func (sa *SemanticAnalyzer) analyzeFactor() DataType {
	// Унарная операция
	if sa.match(TOKEN_TILDE) {
		factorType := sa.analyzeFactor()
		if factorType != TYPE_BOOL && factorType != TYPE_UNKNOWN {
			sa.warning("унарная операция '~' применима только к логическому типу", sa.peek())
		}
		return TYPE_BOOL
	}

	// Логические константы
	if sa.match(TOKEN_TRUE, TOKEN_FALSE) {
		return TYPE_BOOL
	}

	// Числа
	if sa.match(TOKEN_NUMBER) {
		// Определяем тип числа по его значению
		// Упрощенно: если есть точка или E, то float, иначе int
		lexeme := sa.tokens[sa.current-1].Lexeme
		if strings.Contains(lexeme, ".") || strings.Contains(lexeme, "E") || strings.Contains(lexeme, "e") {
			return TYPE_FLOAT
		}
		return TYPE_INT
	}

	// Идентификаторы
	if sa.check(TOKEN_IDENTIFIER) {
		identToken := sa.advance()
		symbol, exists := sa.symbolTable.Get(identToken.Lexeme)
		if !exists {
			sa.error(fmt.Sprintf("переменная '%s' не объявлена", identToken.Lexeme), identToken)
			return TYPE_UNKNOWN
		}
		sa.symbolTable.MarkUsed(identToken.Lexeme)
		return symbol.Type
	}

	// Выражение в скобках
	if sa.match(TOKEN_LPAREN) {
		exprType := sa.analyzeExpression()
		sa.match(TOKEN_RPAREN)
		return exprType
	}

	return TYPE_UNKNOWN
}

func main() {
	if len(os.Args) < 2 {
		testSource := ``
		fmt.Println("╔════════════════════════════════════════════════════════╗")
		fmt.Println("║     ТЕСТИРОВАНИЕ КОМПИЛЯТОРА                           ║")
		fmt.Println("╚════════════════════════════════════════════════════════╝")
		fmt.Println("\nИсходный код:")
		fmt.Println(strings.Repeat("─", 60))
		fmt.Println(testSource)
		fmt.Println(strings.Repeat("─", 60))

		lexer := NewLexer(testSource)
		tokens := lexer.ScanTokens()

		PrintTokensCombined(tokens)

		hasLexicalErrors := false
		for _, token := range tokens {
			if token.Type == TOKEN_ERROR {
				hasLexicalErrors = true
				fmt.Printf("\n[ЛЕКСИЧЕСКАЯ ОШИБКА] Строка %d, столбец %d: %s\n",
					token.Line, token.Column, token.Lexeme)
			}
		}

		if hasLexicalErrors {
			fmt.Println("\n✗ Обнаружены лексические ошибки. Синтаксический анализ прерван.")
			return
		}

		parser := NewParser(tokens)
		parser.Parse()

		return
	}

	filename := os.Args[1]
	source, err := os.ReadFile(filename)
	if err != nil {
		fmt.Printf("Ошибка чтения файла %s: %v\n", filename, err)
		os.Exit(1)
	}

	fmt.Println("╔════════════════════════════════════════════════════════╗")
	fmt.Printf("║     АНАЛИЗ ФАЙЛА: %-36s ║\n", filename)
	fmt.Println("╚════════════════════════════════════════════════════════╝")

	lexer := NewLexer(string(source))
	tokens := lexer.ScanTokens()

	PrintTokensCombined(tokens)

	hasLexicalErrors := false
	for _, token := range tokens {
		if token.Type == TOKEN_ERROR {
			hasLexicalErrors = true
			fmt.Printf("\n[ЛЕКСИЧЕСКАЯ ОШИБКА] Строка %d, столбец %d: %s\n",
				token.Line, token.Column, token.Lexeme)
		}
	}

	if hasLexicalErrors {
		fmt.Println("\n✗ Обнаружены лексические ошибки. Анализ прерван.")
		os.Exit(1)
	}

	// Интегрированный синтаксический и семантический анализ
	parser := NewParser(tokens)
	success := parser.Parse()

	// Вывод таблиц
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     ТАБЛИЦА 2: ИДЕНТИФИКАТОРЫ                          ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	if len(identifierTable) == 0 {
		fmt.Println("(пусто)")
	} else {
		for i, id := range identifierTable {
			fmt.Printf("[%d] %s\n", i, id)
		}
	}

	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     ТАБЛИЦА 3: КОНСТАНТЫ                               ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	if len(constantTable) == 0 {
		fmt.Println("(пусто)")
	} else {
		for i, const_val := range constantTable {
			fmt.Printf("[%d] %s\n", i, const_val)
		}
	}

	if !success {
		os.Exit(1)
	}
}
