#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# q&d test

import sys
import codecs
from subprocess import *

first = True

def pipe_through_prog(prog, text):
    p1 = Popen(prog.split(), stdout=PIPE, stdin=PIPE, stderr=PIPE)
    [result, err] = p1.communicate(input=text.encode('utf-8'))
    return [p1.returncode, result.decode('utf-8'), err]

def get_html(text):
	global first
	if first:
		prog = "cargo run"
		first = False
	else:
		prog = "target/debug/pull-mark-down"
	raw = pipe_through_prog(prog, text)[1]
	return raw.split(': EOF\n')[1]

def main(argv):
	arg = argv[1]
	spl = arg.split('..')
	first = int(spl[0])
	nfail = 0
	if len(spl) == 2:
		last = int(spl[1])
	else:
		last = first
	for i in range(first, last + 1):
		source = codecs.open('t/%d.md' % i, 'r', 'utf-8').read()
		html = codecs.open('t/%d.html' % i, 'r', 'utf-8').read()
		my_html = get_html(source.replace('→',"\t"))
		if html != my_html:
			if nfail == 0:
				print("FAIL %d:" % i)
				print("---input---")
				print(source, end="")
				print('')
				print("---wanted---")
				print(html, end="")
				print('')
				print("---got---")
				print(my_html, end="")
			else:
				print("X", end="", flush=True)
			nfail += 1
		else:
			print(".", end="", flush=True)
	print('')
	print('%d/%d ok' % (last + 1 - first - nfail, last + 1 - first))

main(sys.argv)