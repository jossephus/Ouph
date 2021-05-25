package main

import (
	"testing"
)

func TestPathString(t *testing.T) {
	path := NewPath(".")

	if path.String() != "." {
		t.Errorf("path.String: expected %s. got %s", ".", path.String())
	}
}

func TestAppendString(t *testing.T) {
	path := NewPath(".")

	path.appendString("appended")

	if path.String() != ".appended" {
		t.Errorf("appened path expected %s. got %s", ".appeneded", path.String())
	}
}

func TestPathDirName(t *testing.T) {
	for i, tt := range []struct {
		filename string
		expected string
	}{
		{"a", "a"},
		{"a/b/c.wren", "a/b/c"},
	} {
		path := NewPath(tt.filename)

		dir := path.dirName()

		if dir.String() != tt.expected {
			t.Errorf("TestPathDirName[%d]: want %q, got %q", i, tt.expected, dir.String())
		}
	}
}

func TestPathType(t *testing.T) {
	for _, tt := range []struct {
		path         string
		expectedType PathType
	}{
		{"a.wren", PATH_TYPE_SIMPLE},
		// TODO:	{"/c/users/hp/documents/", PATH_TYPE_ABSOLUTE},
		{"./../../", PATH_TYPE_RELATIVE},
		{"../", PATH_TYPE_RELATIVE},
	} {
		path := NewPath(tt.path)

		if path.PathType() != tt.expectedType {
			t.Errorf("TestPathType[%q]: want %q, got %q", tt.path, tt.expectedType, path.PathType())
		}
	}
}

func TestRemoveExtension(t *testing.T) {
	for _, tt := range []struct {
		path     string
		expected string
	}{
		{"a.wren", "a"},
		{"no/ext", "no/ext"},
		{"a/b/c.wren", "a/b/c"},
	} {
		path := NewPath(tt.path)

		newpath := path.RemoveExtension()

		if newpath.String() != tt.expected {
			t.Errorf("TestRemoveExtension[%q]: want %q, got %q", tt.path, tt.expected, newpath.String())
		}
	}
}

func TestPathJoin(t *testing.T) {
	for _, tt := range []struct {
		path     string
		add      string
		expected string
	}{
		{"a", "b", "a/b"},
		{"a/", "b", "a/b"},
		{"a/b/c", "d/e/f", "a/b/c/d/e/f"},
	} {
		path := NewPath(tt.path)

		newpath := path.Join(tt.add)

		if newpath.String() != tt.expected {
			t.Errorf("TestPathJoin[%q]: want %q, got %q", tt.path, tt.expected, newpath.String())
		}
	}
}

func TestPathNormalize(t *testing.T) {
	for _, tt := range []struct {
		path     string
		join     string
		expected string
	}{
		{"./a/./b/../c", "./d/e/f", "./d/e/a/c"},
	} {
		path := NewPath(tt.path)

		newpath := path.Join(tt.join)

		normalized := newpath.Normalize()

		if normalized.String() != tt.expected {
			t.Errorf("TestPathNormalize[%q]: want %q, got %q", newpath.String(), tt.expected, normalized.String())
		}

	}
}
