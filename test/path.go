package main

import (
	"bytes"
	"path"
	"path/filepath"
)

type Path string

type PathType string

const (
	PATH_TYPE_SIMPLE   PathType = "PATH_SIMPLE_TYPE"
	PATH_TYPE_ABSOLUTE PathType = "PATH_ABSOLUTE_TYPE"
	PATH_TYPE_RELATIVE          = "PATH_TYPE_RELATIVE"
)

func NewPath(path string) Path {
	return Path(path)
}

func (p *Path) String() string {
	return string(*p)
}

func (p *Path) Type() PathType {
	return PATH_TYPE_SIMPLE
}

func (p *Path) Join(add string) Path {
	var bf bytes.Buffer

	bf.WriteString(p.String())

	if len(*p) > 0 && !isSeparator((*p)[len(*p)-1]) {
		bf.WriteByte('/')
	}

	bf.WriteString(add)

	return NewPath(bf.String())
}

func (p *Path) removeExtension() {

}

func (p *Path) appendString(str string) {
	newString := string(*p) + str
	*p = NewPath(newString)
}

func (p *Path) dirName() Path {
	i := 0

	for j := 0; j < len(*p); j++ {
		if (*p)[j] == '.' {
			i = j
		}
	}
	str := p.String()

	return NewPath(str[0 : len(*p)-i])
}

func isSeparator(ch byte) bool {
	return ch == '/' || ch == '\\'
}

func (p *Path) PathType() PathType {
	if filepath.IsAbs(p.String()) {
		return PATH_TYPE_ABSOLUTE
	}

	if ((*p)[0] == '.' && isSeparator((*p)[1])) ||
		(len(*p) > 2 && (*p)[0] == '.' && (*p)[1] == '.' && isSeparator((*p)[2])) {
		return PATH_TYPE_RELATIVE
	}

	return PATH_TYPE_SIMPLE
}

func (p *Path) RemoveExtension() Path {
	i := len(*p) - 1

	for i >= 0 {
		if isSeparator((*p)[i]) {
			return NewPath(p.String())
		}

		if (*p)[i] == '.' {
			break
		}
		i--
	}

	return NewPath(string((*p)[:i]))
}

func (p *Path) Normalize() Path {

	return NewPath(path.Clean(p.String()))
}
