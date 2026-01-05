package main

import (
	"fmt"
	"os"
	"strings"
)

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

// Синтаксический анализатор
type Parser struct {
	tokens  []Token
	current int
	errors  []string
	depth   int // Глубина вложенности для красивого вывода
}

// Создание нового синтаксического анализатора
func NewParser(tokens []Token) *Parser {
	return &Parser{
		tokens:  tokens,
		current: 0,
		errors:  make([]string, 0),
		depth:   0,
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

func (p *Parser) printRule(ruleName string) {
	indent := strings.Repeat("  ", p.depth)
	fmt.Printf("%s→ %s %s\n", indent, ruleName, p.peek().Lexeme)
}

// Главный метод разбора программы
func (p *Parser) Parse() bool {
	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     СИНТАКСИЧЕСКИЙ АНАЛИЗ ПРОГРАММЫ                    ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")

	p.parseProgram()

	if len(p.errors) > 0 {
		fmt.Println("\n╔════════════════════════════════════════════════════════╗")
		fmt.Println("║     ОБНАРУЖЕНЫ СИНТАКСИЧЕСКИЕ ОШИБКИ                   ║")
		fmt.Println("╚════════════════════════════════════════════════════════╝")
		for i, err := range p.errors {
			fmt.Printf("[Ошибка %d] %s\n", i+1, err)
		}
		return false
	}

	fmt.Println("\n╔════════════════════════════════════════════════════════╗")
	fmt.Println("║     ✓ СИНТАКСИЧЕСКИЙ АНАЛИЗ ЗАВЕРШЕН УСПЕШНО           ║")
	fmt.Println("╚════════════════════════════════════════════════════════╝")
	return true
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
	// Может быть 0 или более раз

	for p.check(TOKEN_IDENTIFIER) {
		// Первый идентификатор
		if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор") {
			p.depth--
			return
		}

		// Дополнительные идентификаторы через запятую
		for p.match(TOKEN_COMMA) {
			if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор после ','") {
				p.depth--
				return
			}
		}

		// Двоеточие
		if !p.expect(TOKEN_COLON, "Ожидается ':'") {
			p.depth--
			return
		}

		// Тип данных
		p.parseType()
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
func (p *Parser) parseType() {
	p.printRule("<тип>")
	p.depth++

	if !p.match(TOKEN_INT, TOKEN_FLOAT, TOKEN_BOOL) {
		p.error("Ожидается тип данных (int, float, bool)")
	}

	p.depth--
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
func (p *Parser) parseAssignment() {
	p.printRule("<присваивания>")
	p.depth++

	if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор") {
		p.depth--
		return
	}

	if !p.expect(TOKEN_ASSIGN, "Ожидается ':='") {
		p.depth--
		return
	}

	p.parseExpression()

	p.depth--
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

	p.parseExpression()

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

	p.parseExpression()

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

	if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор") {
		p.depth--
		return
	}

	for p.match(TOKEN_COMMA) {
		if !p.expect(TOKEN_IDENTIFIER, "Ожидается идентификатор после ','") {
			p.depth--
			return
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
func (p *Parser) parseExpression() {
	p.printRule("<выражение>")
	p.depth++

	p.parseOperand()

	for p.match(TOKEN_NE, TOKEN_EQ, TOKEN_LT, TOKEN_LE, TOKEN_GT, TOKEN_GE) {
		p.parseOperand()
	}

	p.depth--
}

// <операнд>::= <слагаемое> {<операции_группы_сложения> <слагаемое>}
func (p *Parser) parseOperand() {
	p.printRule("<операнд>")
	p.depth++

	p.parseTerm()

	for p.match(TOKEN_PLUS, TOKEN_MIN, TOKEN_OR) {
		p.parseTerm()
	}

	p.depth--
}

// <слагаемое>::= <множитель> {<операции_группы_умножения> <множитель>}
func (p *Parser) parseTerm() {
	p.printRule("<слагаемое>")
	p.depth++

	p.parseFactor()

	for p.match(TOKEN_MULT, TOKEN_DIV, TOKEN_AND) {
		p.parseFactor()
	}

	p.depth--
}

// <множитель>::= <идентификатор> | <число> | <логическая_константа> |
//
//	<унарная_операция> <множитель> | (<выражение>)
func (p *Parser) parseFactor() {
	p.printRule("<множитель>")
	p.depth++

	if p.match(TOKEN_TILDE) {
		p.parseFactor()
		p.depth--
		return
	}

	if p.match(TOKEN_TRUE, TOKEN_FALSE) {
		p.depth--
		return
	}

	if p.match(TOKEN_NUMBER) {
		p.depth--
		return
	}

	if p.match(TOKEN_IDENTIFIER) {
		p.depth--
		return
	}

	if p.match(TOKEN_LPAREN) {
		p.parseExpression()
		if !p.expect(TOKEN_RPAREN, "Ожидается ')' после выражения") {
			p.depth--
			return
		}
		p.depth--
		return
	}

	p.error("Ожидается идентификатор, число, логическая константа или '('")
	p.depth--
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

		PrintTokens(tokens)
		PrintTokensAsTableIndex(tokens)

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

	PrintTokens(tokens)
	PrintTokensAsTableIndex(tokens)

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
		os.Exit(1)
	}

	parser := NewParser(tokens)
	success := parser.Parse()

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
