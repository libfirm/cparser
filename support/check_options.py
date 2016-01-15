#!/usr/bin/env python
import re
import sys
import collections

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
              'help_aprefix', 'help_f_yesno' ]
regex = '(?P<function>' + "|".join(functions) + ')'
regex += '\s*\(\s*"(?P<option>[^"]+)"'
help_options = []
for line in helpfile:
    for m in re.finditer(regex, line):
        help_options.append(m.group('option'))

# See which options lack a help string
found_a_problem = False

def print_list(prefix, l):
    global found_a_problem
    intro = False
    for x in l:
        if not intro:
            print prefix,
            intro = True
            found_a_problem = True
        print x,
    if intro:
        print ""

duplicates = [item for (item, count) in collections.Counter(help_options).items() if count > 1]
print_list("Duplicate help:", duplicates)

no_help = set(parsed_options) - set(help_options)
print_list("Options lacking help:", no_help)

no_option = set(help_options) - set(parsed_options)
print_list("Help for nonexistant option:", no_option)

if found_a_problem:
    sys.exit(1)
