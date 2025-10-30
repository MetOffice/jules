#!/usr/bin/env python3

## NOTE ##
# This module is one of several for which the Master copy is in the UM
# repository. When making changes, please ensure the changes are made in the UM
# repository or they will be lost during the release process when the UM copy
# is copied over.

import sys
import re
import argparse
from fstring_parse import *

# These 4 are defined globally for visibility. They are actually only used in
# main or a single subroutine but actual text is not yet defined in stone...

conjoined_keywords = {
        r"BLOCK\s*DATA":          "BLOCK DATA",
        r"DOUBLE\s*PRECISION":    "DOUBLE PRECISION",
        r"ELSE\s*IF":             "ELSE IF",
        r"ELSE\s*WHERE":          "ELSE WHERE",
        r"END\s*ASSOCIATE":       "END ASSOCIATE",
        r"END\s*BLOCK":           "END BLOCK",
        r"END\s*BLOCK\s*DATA":    "END BLOCK DATA",
        r"END\s*CRITICAL":        "END CRITICAL",
        r"END\s*DO":              "END DO",
        r"END\s*ENUM":            "END ENUM",
        r"END\s*FILE":            "END FILE",
        r"END\s*FORALL":          "END FORALL",
        r"END\s*FUNCTION":        "END FUNCTION",
        r"END\s*IF":              "END IF",
        r"END\s*INTERFACE":       "END INTERFACE",
        r"END\s*MODULE":          "END MODULE",
        r"END\s*PARALLEL":        "END PARALLEL",
        r"END\s*PARALLEL\s*DO":   "END PARALLEL DO",
        r"END\s*PROCEDURE":       "END PROCEDURE",
        r"END\s*PROGRAM":         "END PROGRAM",
        r"END\s*SELECT":          "END SELECT",
        r"END\s*SUBMODULE":       "END SUBMODULE",
        r"END\s*SUBROUTINE":      "END SUBROUTINE",
        r"END\s*TYPE":            "END TYPE",
        r"END\s*WHERE":           "END WHERE",
        r"GO\s*TO":               "GO TO",
        r"IN\s*OUT":              "IN OUT",
        r"PARALLEL\s*DO":         "PARALLEL DO",
        r"SELECT\s*CASE":         "SELECT CASE",
        r"SELECT\s*TYPE":         "SELECT TYPE",
}


def split_conjoined_keyword(first_o_line, bit_to_change, new_bit, rest_o_line):
    new_line = first_o_line + new_bit + rest_o_line
    return new_line


def strip_trailing_space(line):
    return re.sub(r"\s*$", "", line)


def keyword_split(line, str_continuation):

    if len(line.strip()) == 0:
        return line

    workline = clean_str_continuation(line, str_continuation)

    stripline = workline.strip()

    # Pre-processor lines start with #. Ignore them completely.
    pre_proc = (stripline[0] == "#")

    # Lines that are completely commented start with a bang and are also
    # ignored completely.
    all_comment = (stripline[0] == "!")

    if pre_proc or all_comment:
        return line

    # remove strings
    try:
        blank_line = blank_fstring(workline)
    except ParsingError as e:
        str_cont_test = is_str_continuation(workline)
        check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
        if check_quote is True:
            blank_line = partial_blank_fstring(workline)
        else:
            print("keyword split simplify line has failed.")
            print("{0:s} Line simplification has failed for:".format(e.msg))
            print(line)
            exit(1)

    # remove comments
    match_line = blank_fcomments(blank_line)

    # Look for possible keyword missing a space...
    for key, value in conjoined_keywords.items():
        searchstring = r"(^.*\W|^)(" + key + r")(\W.*$|$)"

        m = re.search(searchstring, match_line, re.IGNORECASE)
        if m:
            first_o_line = line[m.start(1):m.end(1)]
            bit_to_change = line[m.start(2):m.end(2)]
            rest_o_line = line[m.start(3):]
            line = split_conjoined_keyword(first_o_line, bit_to_change,
                                           value, rest_o_line)

            workline = clean_str_continuation(line, str_continuation)

            # remove strings
            try:
                blank_line = blank_fstring(workline)
            except ParsingError as e:
                str_cont_test = is_str_continuation(workline)
                check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
                if check_quote is True:
                    blank_line = partial_blank_fstring(workline)
                else:
                    print("keyword split simplify line has failed.")
                    print("{0:s} Line simplification has failed " \
                          "for:".format(e.msg))
                    print(line)
                    exit(1)

            # remove comments
            match_line = blank_fcomments(blank_line)

    return line


def apply_whitespace_fixes(lines, striptrailingspace=True, keywordsplit=True):
    '''For a lit of lines apply UMDP3 styling to each line and return
    the result'''

    output_lines = []
    continuation = False
    pp_continuation = False
    str_continuation = [False, False]
    pseudo_str_continuation = [False, False]
    pseudo_comment = False
    pp_line_previous = ""
    line_previous = ""

    for iline, line in enumerate(lines):
        if striptrailingspace:
            # Strip out trailing spaces ?
            line = strip_trailing_space(line)

        if keywordsplit:
            if pp_continuation:
                if not pseudo_comment:
                    line = keyword_split(line, pseudo_str_continuation)
            else:
                line = keyword_split(line, str_continuation)

        # Check for the start of new pre-processor continuation
        if is_pp_continuation(line) and not pp_continuation:
            pp_continuation = True

        # test the line continuation properties of this line
        if pp_continuation:
            if not is_pp_continuation(line):
                pp_continuation = False
                pp_line_previous = ""
                pseudo_comment = False
        elif continuation:
            if is_continuation(line, str_continuation):
                # check if still string continuation
                str_continuation = is_str_continuation(line, str_continuation)
            else:
                continuation = False
                line_previous = ""
                str_continuation = [False, False]
        else:
            # Finally, detect if the following line is a continuation
            # of this one (and therefore requires no indentation)
            if is_continuation(line, str_continuation):
                continuation = True
                str_continuation = is_str_continuation(line, str_continuation)

        # if we are a (pp) continuation, save the partial line
        if pp_continuation:
            pp_line_previous = ''.join([re.sub(r"\\\s*$", "",
                                               pp_line_previous),
                                        re.sub(r"&\s*$", "", line_previous),
                                        line])
            line_previous = ""
            pseudo_line = re.sub(r"\\\s*$", "&", pp_line_previous)
            pseudo_str_continuation = is_str_continuation(pseudo_line,
                                                          str_continuation)
            if not pseudo_comment:
                pseudo_line = partial_blank_fstring(pseudo_line,
                                                    str_continuation)
                if pseudo_line.strip()[0] == "#":
                    pseudo_comment = True
                if pseudo_line.find("!") != -1:
                    pseudo_line = blank_fcomments(pseudo_line,
                                                  str_continuation)
                    if pseudo_line.find("!") == -1:
                        pseudo_comment = True
        elif continuation:
            line_previous = re.sub(r"&\s*$", "", line_previous)
            line_previous += blank_fcomments(line, str_continuation)

        output_lines.append(line)

    return output_lines


def main():
    '''
    Main program code.
    Expects there to be a single command line argument, which should be the
    name of a file to process. It will abort if no argument is provided, or
    the file cannot open.
    In processing the file it will attempt to add a USE statement for the
    module defining the KIND type variable if none exists. It will do this
    at a Module level to save having to keep track of each and every
    subroutine a file may contain. NOTE - if your file doesn't contain
    a module - you're in trouble...
    Then each lie is checked to see if there is a 'REAL' declaration and
    processed if there is.
    '''
    parser = argparse.ArgumentParser()
    parser.add_argument("-k", "--keywordsplit",
                        help="Split potentially conjoined keywords"
                             " such as INOUT",
                        action="store_true")
    parser.add_argument("-s", "--striptrailingspace",
                        help="Strip Trailing spaces from lines",
                        action="store_true")
    parser.add_argument("filename", type=str,
                        help="Name of file to process")
    args = parser.parse_args()

    modify = False

    if args.keywordsplit:
        print("Splitting of potentially conjoined keywords turned on")
        modify = True
    else:
        print("Splitting of potentially conjoined keywords turned off")
    if args.striptrailingspace:
        print("stripping trailing spaces turned on")
        modify = True
    else:
        print("stripping trailing spaces turned off")

    if not modify:
        print("No options selected that would cause any code changes.")
        print("Exiting, as running would have no effect.")
        exit(1)

    filename = args.filename
    try:
        with open(filename) as fortran_file:
            raw_code = fortran_file.readlines()
    except EnvironmentError:
        print("Error opening file. :\n     \"{0:s}\"\n".format(filename) +
              "I need a valid filename on which to work....")
        raise SystemExit

    print("\nLooking at file :\n     {0:s}".format(filename))
    # re-open the fortran file, this time to write to it.
    with open(filename, 'r+') as fortran_file:
        lines_in = fortran_file.read().split("\n")
        new_lines = apply_whitespace_fixes(lines_in, args.striptrailingspace,
                                           args.keywordsplit)
        fortran_file.seek(0)
        fortran_file.write("\n".join(new_lines))
        fortran_file.truncate()


if __name__ == "__main__":
    main()
