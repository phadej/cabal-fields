#!/usr/bin/env python3

import cabalfields
import sys
import argparse
import pprint

# AST
##############################################################################

class FieldLine:
    def __init__(self, contents, pos):
        self.contents = contents
        self.pos = pos

    def __str__(self):
        return repr(self.contents)

    def __repr__(self):
        return f"FieldLine({self.contents})"

    def pp(self, printer):
        printer.at(self.pos, self.contents)
        printer.nl()

class Comment:
    def __init__(self, contents, pos):
        self.contents = contents
        self.pos = pos

    def __str__(self):
        return repr(self.contents)

    def __repr__(self):
        return f"Comment({self.contents})"

    def pp(self, printer):
        row, col = self.pos
        col = max(1, col - 2)
        printer.at((row, col), b'--')
        printer.at(self.pos, self.contents)

class Field:
    def __init__(self, name, name_pos, colon_pos, contents):
        self.name = name
        self.name_pos = name_pos
        self.colon_pos = colon_pos
        self.contents = contents

    def __repr__(self):
        return f"Field({self.name}, {self.contents})"

    def pp(self, printer):
        printer.at(self.name_pos, self.name)
        printer.at(self.colon_pos, b":")
        for sub in self.contents:
            sub.pp(printer)

class Section:
    def __init__(self, name, name_pos, args, contents):
        self.name = name
        self.name_pos = name_pos
        self.args = args
        self.contents = contents

    def __repr__(self):
        return f"Section({self.name}, {self.args}, {self.contents})"

    def pp(self, printer):
        printer.at(self.name_pos, self.name)
        if self.args:
          printer.here(b' ')
          printer.here(self.args)
        for sub in self.contents:
            sub.pp(printer)

# Pretty-printer
##############################################################################

class Printer:
    def __init__(self):
        self.builder = []
        self.curr    = (1, 1)

    def output(self):
        return b"".join(self.builder)

    def eof(self):
        curr_row, curr_col = self.curr
        if curr_col > 1:
            self.nl()

    def at(self, pos, contents):
        self.__whitespace(pos)
        self.here(contents)

    def nl(self):
        self.builder.append(b"\n")
        curr_row, curr_col = self.curr
        self.curr = (curr_row + 1, 1)

    def __whitespace(self, pos):
        curr_row, curr_col = self.curr
        next_row, next_col = pos

        if next_row > curr_row:
            curr_col = 1
            while next_row > curr_row:
                self.builder.append(b"\n")
                curr_row += 1

        while next_col > curr_col:
                self.builder.append(b" ")
                curr_col += 1

        self.curr = pos

    def here(self, string):
        curr_row, curr_col = self.curr
        self.builder.append(string)
        self.curr = (curr_row, curr_col + len(string))

def pp(asts):
    printer = Printer()
    for ast in asts:
        ast.pp(printer)
    printer.eof()
    return printer.output()

# Parser
##############################################################################

def parse_top(tokens):
    acc = []
    while True:
        try:
            t = next(tokens)
        except StopIteration:
            return acc

        if t['type'] == "section":
            acc.append(parse_section(t['name'], t['name_pos'], t['args'], tokens))
            continue
        elif t['type'] == 'field':
            acc.append(parse_field(t['name'], t['name_pos'], t['colon_pos'], tokens))
            continue
        elif t['type'] == 'comment':
            acc.append(Comment(t['comment'], t['comment_pos']))
            continue
        else:
            raise Exception(str(t))

def parse_section(name, name_pos, args, tokens):
    acc = []
    while True:
        t = next(tokens)
        if t['type'] == "section":
            acc.append(parse_section(t['name'],  t['name_pos'], t['args'], tokens))
            continue
        elif t['type'] == 'field':
            acc.append(parse_field(t['name'], t['name_pos'], t['colon_pos'], tokens))
            continue
        elif t['type'] == 'section_end':
            return Section(name, name_pos, args, acc)
        elif t['type'] == 'comment':
            acc.append(Comment(t['comment'], t['comment_pos']))
            continue
        else:
            raise Exception(str(t))

def parse_field(name, name_pos, colon_pos, tokens):
    acc = []
    while True:
        t = next(tokens)
        if t['type'] == 'field_end':
            return Field(name, name_pos, colon_pos, acc)
        elif t['type'] == 'fieldline':
            acc.append(FieldLine(t['line'], t['line_pos']))
            continue
        elif t['type'] == 'comment':
            acc.append(Comment(t['comment'], t['comment_pos']))
            continue
        else:
            raise Exception(str(t))

# Demo
##############################################################################

def print_file_tokens(filename):
    with open(filename, mode="rb") as f:
        contents = f.read();
        tokens = cabalfields.Tokens(contents)

        for t in tokens:
            print(t)

def action_module_help(args):
    help(cabalfields)
    return 0

def action_print_tokens(args):
    for filename in args.filename:
        print_file_tokens(filename)

    return 0

def action_print_tree(args):
    for filename in args.filename:
        with open(filename, mode="rb") as f:
            contents = f.read();
            tokens = cabalfields.Tokens(contents)

            res = parse_top(tokens)
            pprint.pprint(res)

def normalise(bs):
    ls = bs.splitlines()
    return b'\n'.join(ls)

def action_exact_print(args):
    for filename in args.filename:
        with open(filename, mode="rb") as f:
            print(filename)
            contents = f.read()
            tokens = cabalfields.Tokens(contents)

            res = parse_top(tokens)
            res_pp = pp(res)
            print("??? same:", normalise(contents) == normalise(res_pp))
            sys.stdout.buffer.write(res_pp)

def main():
    parser = argparse.ArgumentParser(prog='cabalfields', description='A demo program of cabalfields module')
    parser.add_argument('filename', nargs='*', help='Input files')
    parser.add_argument('--module-help',  dest='action', action='store_const', const=action_module_help)
    parser.add_argument('--print-tokens', dest='action', action='store_const', const=action_print_tokens)
    parser.add_argument('--print-tree',   dest='action', action='store_const', const=action_print_tree)
    parser.add_argument('--exact-print',  dest='action', action='store_const', const=action_exact_print)
    args = parser.parse_args()

    if args.action:
        return args.action(args)

    return action_exact_print(args)

if __name__ == '__main__':
    sys.exit(main())
