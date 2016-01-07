#!/usr/bin/env python
import re

# Search for defined options
optionsfile = open("src/driver/options.c").readlines() + open("src/main.c").readlines()
functions = [ 'simple_arg', 'prefix_arg', 'spaced_arg', 'equals_arg',
              'f_yesno_arg' ]
regex = '(?P<function>' + "|".join(functions) + ')'
regex += '\s*\(\s*"(?P<option>[^"]+)"'
parsed_options = []
for line in optionsfile:
    for m in re.finditer(regex, line):
        option = m.group('option')
        function = m.group('function')
        if function != 'f_yesno_arg':
            option = '-' + option
        parsed_options.append(option)
    # accept_prefix() takes s as first argument
    for m in re.finditer('accept_prefix\s*\([^,]*,\s*"(?P<option>[^"]+)', line):
        option = m.group('option')
        parsed_options.append(option)

# Search for help strings
helpfile = open("src/driver/help.c")
functions = [ 'help_simple', 'help_prefix', 'help_spaced', 'help_equals',
              'help_aprefix' ]
regex = '(?P<function>' + "|".join(functions) + ')'
regex += '\s*\(\s*"(?P<option>[^"]+)"'
help_options = []
for line in helpfile:
    for m in re.finditer(regex, line):
        help_options.append(m.group('option'))

# See which options lack a help string
print "Options lacking help:",
for x in parsed_options:
    if x not in help_options:
        print x,
print "\n"

print "Help for nonexistant option:",
for x in help_options:
    if x not in parsed_options:
        print x,
print ""
