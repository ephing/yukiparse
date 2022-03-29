package lexer
import "fmt"

type Token struct {
	name   string
	lexeme []rune
	pos    [2]int
}

type Action struct {
	acType   string
	next     string
	reduceBy int
}

type Parser[T any] struct {
	table      map[string]map[string]Action
	start      string
	semActions map[string]func([]rune, []T) []T
}

func Parse[T any](parser Parser[T], tkstream []Token) (T, bool) {
	stack := []string{parser.start}
	lookahead := 1
	semstack := []T{}
	for len(stack) != 0 {
		next := parser.table[stack[0]][tkstream[lookahead].name]
		if next.acType == "Accept" {
			return semstack[0], true
		}
		if next.acType == "Shift" {
			stack = append([]string{next.next}, stack...)
			lookahead += 1
			continue
		}
		if next.acType == "Reduce" {
			if parser.table[stack[next.reduceBy]][next.next].acType == "Shift" {
				stack = append([]string{parser.table[stack[next.reduceBy]][next.next].next}, stack[next.reduceBy:]...)
				continue
			}
			fmt.Println("Parse Error: Invalid Reduce is Specification (" + next.next + ", " + parser.table[stack[next.reduceBy]][next.next].next + ")")
			return semstack[0], false
		}
		fmt.Println("Parse Error: Invalid Transition (" + stack[0] + ", " + tkstream[lookahead].name + ")")
		return semstack[0], false
	}
	fmt.Println("Parse Error: Unexpected EOF")
	return semstack[0], false
}
