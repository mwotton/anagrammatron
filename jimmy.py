
#!/usr/bin/env python
#
# Example usage:
#  python nam_a_gra.py james boyden

WORDS_FNAME = "/usr/share/dict/words"
TWO_LETTER_NAMES = set("al,di,ed,jo,li".split(','))
SURNAME_PREFIXES = set("al,de,di,le,li,van,von".split(','))

import re
import sys
from itertools import permutations

NAMES_REGEX = re.compile("([A-Z][a-z][a-z]+)\\s$")  # 3+-letter names
NAMES = set(w.group(1).lower()
        for w in (NAMES_REGEX.match(line) for line in open(WORDS_FNAME))
        if w is not None)

SURNAMES = NAMES | SURNAME_PREFIXES
NAMES = NAMES | TWO_LETTER_NAMES

ARGS = [w.lower() for w in sys.argv[1:]]
NAME = " ".join(ARGS)
NAME_NO_SPACES = "".join(ARGS)

# Need to keep track of the results, with comparison by character value, to
# avoid printing duplicates when a character appears twice in the input name.
# This is because the permutation considers characters to be unique by position
# rather than by value.  So if a character appears twice in the input name, any
# results would be printed twice, once for each of the identical characters.
# Also, don't bother printing out the original name.
NAMES_FOUND = set([NAME])

def print_result(good_names):
    result_name = " ".join(good_names)
    if result_name not in NAMES_FOUND:
        print result_name
        sys.stdout.flush()
        NAMES_FOUND.add(result_name)

def find_surname(good_names, remainder):
    if remainder == "":
        print_result(good_names)
        return

    for j in xrange(2, len(remainder)+1):
        if remainder[:j] not in SURNAMES:
            continue
        good_name = remainder[:j]
        good_names2 = good_names[:]  # own copy for recursion
        good_names2.append(good_name)
        find_surname(good_names2, remainder[j:])

r = range(2, len(NAME_NO_SPACES)+1)
for p_ in permutations(NAME_NO_SPACES):
    p = "".join(p_)  # The permutation of a string returned as a tuple. Dumb.
    for i in r:
        if p[:i] not in NAMES: 
            continue
        first_name = p[:i]
        find_surname([first_name], p[i:])
