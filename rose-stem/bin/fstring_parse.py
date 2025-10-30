# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
'''
## NOTE ##
This module is one of several for which the Master copy is in the
UM repository. When making changes, please ensure the changes are made in the UM
repository or they will be lost during the release process when the UM copy is
copied over.

This module contains various functions for parsing and manuipulating
quoted strings in Fortran
'''
import re


class ParsingError(Exception):
    '''
    Raised when an operation attempts to parse a line that doesn't look
    like it expects.
    '''
    def __init__(self):
        self.msg = 'Parsing Error.'
    pass


class QuoteError(ParsingError):
    '''
    Raised when there are an unexpected number of quote marks (single or
    double) in a line.
    '''
    def __init__(self, quote, number):
        self.number = number
        self.quote = quote
        self.quotemarks = {"'": "single quotes",
                           "\"": "double quotes"}

        self.msg = "There are an odd number of non-commented " \
                   "and non-quoted {1:s} in this line. " \
                   "(From a total of {0:d})".format(number,
                                                    self.quotemarks[quote])
    pass


SQUOTE = 0
DQUOTE = 1

PBLANKFSTRRE = re.compile("^[^!]*?('|\")")
BLANKFCOMSRE = re.compile("('|\")(.*)!(.*)&")
ISPPCONTCR = re.compile(r"\\\s*$", flags=re.IGNORECASE)


def replace_characters(line, locs, lens, replchar="X"):
    '''
    Replace characters in a line at particular locations.

    e.g. for the line
    > line = "She said 'I like tea at Fortnum & Mason'. I prefer fish & chips."

    And locations and lengths
    > locs = [32, ]
    > lens = [1, ]

    The result is
    > newline = replace_characters(line, locs, lens, replchar="+"):
    > print newline
    She said 'I like tea at Fortnum + Mason'. I prefer fish & chips.

    '''

    # This code requires that the replacement is a single string, which is
    # used as many times as necessary.
    if len(replchar) != 1:
        raise ValueError("Argument 'replchar' must be a single character")

    # Split the line into a list, as characters can't be changed in place in
    # a string.
    newline = list(line)

    # Loop through locations and lengths and replace each character with the
    # replacement character character.
    for loc, ln in zip(*[locs, lens]):
        for l in range(ln):
            newline[loc+l] = replchar

    # Return the newline, joined back together as a string.
    return "".join(newline)


def blank_fstring(line, string_continuation=[False, False]):
    "blanks out strings within the fortran line"

    # partially blank the strings
    bline = partial_blank_fstring(line, string_continuation)

    # if we failed to get all the single or double quotes that aren't in a
    # comment, something has gone wrong
    match = PBLANKFSTRRE.search(bline)

    if match is not None:
            # match character is a solo - something has gone wrong
            raise QuoteError(match.group(1), len(re.findall(match.group(1),
                                                            line)))

    return bline


def partial_blank_fstring(line, string_continuation=[False, False]):
    "blanks out strings within the fortran line"

    bline = clean_str_continuation(line, string_continuation)

    if not PBLANKFSTRRE.search(bline):
        # all strings are after the start of comments; return the original line
        return bline

    while 1:
        finder = bline.find

        # find the first remaining '
        apos_loc = finder("'")

        # find the first remaining "
        quot_loc = finder("\"")

        if (apos_loc == -1 and quot_loc == -1):
            # no strings remaining
            break

        if (apos_loc == -1):
            apos_loc = quot_loc + 1
        if (quot_loc == -1):
            quot_loc = apos_loc + 1

        first_loc = min(apos_loc, quot_loc)

        # find the first remaining !
        bang_loc = finder("!")

        if (bang_loc != -1 and bang_loc < first_loc):
            # all remaining strings are after the start of comments
            break

        matchchar = line[first_loc]

        match = re.search(matchchar+'.*?'+matchchar, bline)

        if match is not None:
            start = match.start()
            end = match.end()
            bline = replace_characters(bline, [start], [end-start],
                                       replchar=' ')
        else:
            # match character is a solo - we have done as much as we can
            break

    return bline


def blank_fcomments(line, string_continuation=[False, False]):
    "blanks out comments within the fortran line"

    bline = line

    # find the quoted strings to avoid non-comment ! characters
    modified_line = partial_blank_fstring(line, string_continuation)

    # mop up potential string continuation containing ! characters
    while BLANKFCOMSRE.search(modified_line, re.IGNORECASE):
        modified_line = BLANKFCOMSRE.sub(r"\1\2 \3&", modified_line)

    # any leftover ! must be comments
    if "!" in modified_line:
        start = modified_line.index("!")
        end = len(bline)
        bline = replace_characters(bline, [start], [end-start], replchar=' ')

    return bline


def is_continuation(line, string_continuation=[False, False]):
    "checks if line is a continuation line"

    # Now remove any strings and comments to simplify later parsing.
    # (Prevent false &, ', or " character matches from strings and comments)
    parblanked = partial_blank_fstring(line, string_continuation)
    parblanked = blank_fcomments(parblanked)

    # The line may have a string continuation. If it's a
    # string continuation, it follows it's a regular continuation too.
    str_cont_test = is_str_continuation_preparblank(parblanked, line)
    if str_cont_test[SQUOTE] is True or str_cont_test[DQUOTE] is True:
        return True

    cont = False

    # ignore pp lines - they can have & characters in the context
    # or logical && tests
    strip_modify = parblanked.strip()
    if len(strip_modify) > 0:
        if strip_modify[0] == "#":
            return False

    if "&" in parblanked:
        cont = True

    return cont


def is_pp_continuation(line):
    "checks if line is a pre-processing continuation line"

    if ISPPCONTCR.search(line):
        return True

    return False


def is_str_continuation_preparblank(parblanked, line):
    """worker routine to check if line is a string continuation without
    directly calculating the blanked line (for performance reasons)
    """
    cont = [False, False]

    finder = parblanked.find

    # any & characters left over are continuations.
    # If there are no continuations, there can be no string continuation
    if finder("&") == -1:
        return cont

    # pp lines cannot be string continuations
    strip_modify = parblanked.strip()
    if len(strip_modify) > 0:
        if strip_modify[0] == "#":
            return cont

    # find the first remaining '
    apos_loc = finder("'")

    # find the first remaining "
    quot_loc = finder("\"")

    if (apos_loc == -1 and quot_loc == -1):
        # no strings remaining; ergo no string continuation
        return cont

    # find which of ' or " occurs first
    if (apos_loc == -1):
        apos_loc = quot_loc + 1
    if (quot_loc == -1):
        quot_loc = apos_loc + 1

    first_loc = min(apos_loc, quot_loc)

    # set the correct return based on the first occurance character
    if line[first_loc] == "'":
        cont[SQUOTE] = True
    else:
        cont[DQUOTE] = True

    return cont


def is_str_continuation(line, string_continuation=[False, False]):
    "checks if line is a string continuation"

    # Now remove any strings and comments to simplify later parsing.
    # (Prevent false &, ', or " character matches from strings and comments)
    parblanked = partial_blank_fstring(line, string_continuation)
    parblanked = blank_fcomments(parblanked)

    return is_str_continuation_preparblank(parblanked, line)


def clean_str_continuation(line, string_continuation=[False, False]):
    "blanks out strings withing the fortran line"

    # If we are string continuation, first deal with the leading partial string
    if string_continuation[SQUOTE]:
        out_line = re.sub("^(.*?)'", r"\1 ", line)
    elif string_continuation[DQUOTE]:
        out_line = re.sub("^(.*?)\"", r"\1 ", line)
    else:
        out_line = line

    return out_line


def simplify_line(lines):
    "A pre-processor for the lines to make them easier to handle"

    # Note we will be passed a slice to include the lines after the
    # current line to the end of the file to allow handling of
    # continutations
    iline = 0
    line = lines[iline]

    repeat_simplify = False

    if is_pp_continuation(line):
        repeat_simplify = True
    elif is_continuation(line):
        repeat_simplify = True

    # Pull any continuation lines into this line
    while repeat_simplify:

        while is_pp_continuation(line):
            iline += 1
            line = ''.join([re.sub(r"\\(\s*)$", r" \1", line), lines[iline]])

        # blank any strings pulled in, in case they contain a ! character
        try:
            line = blank_fstring(line)
        except ParsingError as e:
            str_cont_test = is_str_continuation(line)
            if not (str_cont_test[SQUOTE] or str_cont_test[DQUOTE]):
                print("Indentation simplify line has failed. [2]")
                print("{0:s} Line simplification has failed for:".format(e.msg))
                print(line)
                exit(1)

        # Blank out possible trailing comments
        if "!" in line:
            line = blank_fcomments(line)

        # If this results in an empty line we are done
        if line.strip() == "":
            return line

        iline += 1

        while 1:
            xiline = iline
            xline = lines[xiline]

            while is_pp_continuation(xline):
                xiline += 1
                xline = ''.join([re.sub(r"\\(\s*)$", r" \1", xline),
                                lines[xiline]])

            xline = clean_str_continuation(xline, is_str_continuation(line))

            # Skip following lines if they contain only comments,
            # pre-processor directives, or are empty
            if (re.search(r"^\s*$", blank_fcomments(xline),
                          flags=re.IGNORECASE)
               or re.search(r"^\s*#\w+", xline, flags=re.IGNORECASE)):
                iline = xiline + 1
            else:
                break

        line = ''.join([re.sub(r"&(\s*)$", r" \1", line), lines[iline]])

        if not is_pp_continuation(line):
            if not is_continuation(line):
                repeat_simplify = False

    # if the line still continues in some form, we have mis-parsed
    if is_continuation(line) or is_pp_continuation(line):
        print("Indentation simplify line has failed. [3]")
        print("Line still appears to have continuations after parsing. " \
              "Line simplification has failed for:")
        print(line)
        exit(1)

    # Repeat the substitution for strings (in case any were pulled
    # into the string above)
    try:
        line = blank_fstring(line)
    except ParsingError as e:
        print("Indentation simplify line has failed. [3]")
        print("{0:s} Line simplification has failed for:".format(e.msg))
        print(line)
        exit(1)

    # Comments - blank out any comments on the trailing line too
    if "!" in line:
        line = blank_fcomments(line)

    # Brackets - remove any nested brackets
    # (i.e. leave only top level brackets)
    # this is to aid with pattern matching where brackets are included
    bracket_nest_level = 0
    new_line = ""
    for char in (line):
        if char == "(":
            bracket_nest_level += 1
            if bracket_nest_level > 1:
                new_line += " "
                continue
        if char == ")":
            if bracket_nest_level > 1:
                new_line += " "
                bracket_nest_level -= 1
                continue
            bracket_nest_level -= 1
        new_line += char

    line = new_line
    return line


def find_quoted_char(line, char, string_continuation=[False, False]):
    '''
    Check if a particular string (char) is present inside double or single
    quotes within a line of text, and return locations of any occurrences
    within the line.

    e.g. for a line containing a quote,
    > line = "She said 'I like tea at Fortnum & Mason'. I prefer fish & chips."
                                            ^
    The function

    > locs = find_quoted_char(line, "&")

    will return the location of the first ampersand

    > print locs
    [32,]

    as it is within the single quoted speech. The location of the second
    ampersand is not returned, as it is not within a quote in the line.

    Note that this assumes that the line has been pre-processed to remove
    apostrophes. It will fail if there are an odd number of single or
    double quotes on the line.
    '''

    # First check there are any instances of char in this line. If not, just
    # leave without doing anything.
    if re.search(char, line) is None:
        return None

    # Then check if there are any quote marks. If not, just leave without doing
    # anything
    if re.search("['\"]", line) is None:
        return None

    try:
        blanked_str = blank_fstring(line, string_continuation)
    except QuoteError:
        str_cont_test = is_str_continuation(line, string_continuation)
        if not (str_cont_test[SQUOTE] or str_cont_test[DQUOTE]):
            raise
        blanked_str = partial_blank_fstring(line, string_continuation)

    char_loc = []

    # If the character is quoted, it will have been removed from the
    # blanked_str version. Therefore, any difference between the strings at the
    # location of the character in the original is a quoted character location.
    for match in re.finditer(char, line):
        i = match.start()
        if (line[i] != blanked_str[i]):
            char_loc.append(i)

    if len(char_loc) > 0:
        return char_loc
    else:
        return None


def find_commented_char(line, char, string_continuation=[False, False]):
    r'''
    Check if a particular string (char) is present inside a Fortran comment
    within a line of text, and return locations of any occurrences within the
    line.

    e.g. for a line of Fortran code, say we want to find the opening
    parenthesis in the comment
    > line = "INTEGER, INTENT(IN) :: land_pts    ! No. land points (to run)"
                                                             ^
    The function

    > locs = find_commented_char(line, r"\(")

    will return the location of the parenthesis in the comment

    > print locs
    [53,]

    as it is within the commented text. The location of the parenthesis
    around IN is not returned, as it before the quote starts.

    Note that this will give unexpected results if there are bangs that aren't
    comment markers. It assumes that the line has been pre-processed with
    find_quoted_char to remove any bangs that are within strings.
    '''

    # First check there are any instances of char, if not just leave
    # without processing the line.
    if re.search(char, line) is None:
        return None

    # Now check if there are comments in this line. Note that this assumes that
    # any bang present is a comment indicator. If there are any others (e.g.
    # within quoted text), then this may fail. It is recommended to pre-process
    # the line to make sure that the first bang on the line indicates the start
    # of a comment. In normal usage, run find_quoted_char before
    # find_commented_char to ensure it isn't a problem.
    if re.search("!", line) is None:
        # If there are no bangs, we can go.
        return None

    try:
        blanked_line = blank_fstring(line, string_continuation)
    except QuoteError:
        str_cont_test = is_str_continuation(line, string_continuation)
        if not (str_cont_test[SQUOTE] or str_cont_test[DQUOTE]):
            raise
        blanked_line = partial_blank_fstring(line, string_continuation)

    # Find comment location, but only if there is actually something after the
    # bang. If not, then there is no actual comment.
    # Note that this assumes that the first bang that is found is the start of
    # a comment (it assumes that any within quotes have already been dealt
    # with).
    comment = re.search("!.+", blanked_line)

    # If there is a comment, find any instances of the char in the comment.
    if comment is None:
        char_loc = []
    else:
        char_loc = [loc.start() + comment.start()
                    for loc in re.finditer(char, comment.group())]

    # Return the location of any commented characters.
    if len(char_loc) > 0:
        return char_loc
    else:
        return None
