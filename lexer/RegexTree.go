package lexer

type Tree interface {
	toStr(int) string
}

type Concat struct {
	ch1 Tree
	ch2 Tree
}

type Split struct {
	ch1 Tree
	ch2 Tree
}

type Kleene struct {
	ch Tree
}

type CharSel struct {
	chars []rune
}

type Prim struct {
	primitive  rune
	isWildcard bool
}

func (tree *Concat) toStr(tabs int) (out string) {
	for i := 0; i < tabs; i++ {
		out += "\t"
	}
	out += "Concat:\n" + tree.ch1.toStr(tabs+1) + "\n" + tree.ch2.toStr(tabs+1) + "\n"
	return
}

func (tree *Split) toStr(tabs int) (out string) {
	for i := 0; i < tabs; i++ {
		out += "\t"
	}
	out += "Split:\n" + tree.ch1.toStr(tabs+1) + "\n" + tree.ch2.toStr(tabs+1) + "\n"
	return
}

func (tree *Kleene) toStr(tabs int) (out string) {
	for i := 0; i < tabs; i++ {
		out += "\t"
	}
	out += "Kleene:\n" + tree.ch.toStr(tabs+1) + "\n"
	return
}

func (tree *CharSel) toStr(tabs int) (out string) {
	for i := 0; i < tabs; i++ {
		out += "\t"
	}
	out += "CharSel: " + string(tree.chars)
	return
}

func (tree *Prim) toStr(tabs int) (out string) {
	for i := 0; i < tabs; i++ {
		out += "\t"
	}
	out += "Prim: " + string(tree.primitive)
	return
}
