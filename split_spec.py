import sys

i = 0
state = 0
for l in sys.stdin:
	if l == '.\n':
		if state == 0:
			i += 1
			f = file('t/%d.md' % i, 'w')
			state = 1
		elif state == 1:
			f.close()
			f = file('t/%d.html' % i, 'w')
			state = 2
		elif state == 2:
			f.close()
			state = 0
	elif state:
		f.write(l)
