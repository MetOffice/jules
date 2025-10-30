#!/usr/bin/env python3
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

This module contains various functions for putting continuation ampersands in
the same column throughout FORTRAN source code.

If there are comments after the continuation ampersand, then the whitespace is
adjusted as much as possible to reduce line to the required length.

Lines that are still too long will be identified, optionally with a message in
stdout.

Note that this code has made an effort to deal with the possibility of
amersands and exclamation marks within quoted strings or comments, but there
may be some cases which are missed. These lines will be left without applying
the ampersand shifting, and will be flagged, optionally with a message in
stdout.
'''
import sys
import re
import traceback
from optparse import OptionParser
from fstring_parse import *

# Default column in which to place ampersands.
DEFAULT_COL = 80


class CharError(ParsingError):
    '''
    Raised when there are an unexpected number of a certain char in a line.
    '''
    def __init__(self, char, number):
        self.number = number
        self.char = char
        self.msg = "There are {0:d} unquoted, uncommented " \
                   "\"{1:s}\" in this line.".format(number, char)
    pass


def print_message(errtype, msg, iline=None, line=None, fname=None):
    '''
    Print a formatted message
    '''
    if fname is None:
        fnamestr = ""
    else:
        fnamestr = "{0:s}:".format(fname)

    if iline is None:
        if fnamestr is None:
            ilinestr = ""
        else:
            ilinestr = " "
    else:
        ilinestr = "L{0:05d}: ".format(iline)

    if line is None:
        linestr = ""
    else:
        linestr = ": {0:s}".format(line)

    print("{0:s}{1:s}{2:s} - {3:s}{4:s}".format(fnamestr, ilinestr, errtype,
                                                msg, linestr))


def shift_ampersand(line, line_previous, str_continuation, col=DEFAULT_COL,
                    preclean=False):
    '''
    Check if the line contains an ampersand.
    If so then set location of ampersand to col so as to be consistent
    Sometimes there are comments after the ampersand, in this case keep
    the comment and ampersand where they are, unless the line is too long, in
    which case reduce any whitespace between the ampersand and the
    comment. If the line is still too long, then reduce whitespace between
    the end of the code and the ampersand until the comment fits within the
    required line length.
    '''

    # return earliy if there are no apersands at all.
    if "&" not in line:
        return line

    if (len(re.findall("&", line)) < 2) and preclean is True:
        return line

    lp = len(line_previous)

    if lp > 0:
        workline = re.sub(r"\\(\s*)$", r" \1", line_previous) + line
    else:
        workline = line

    col = col + lp

    stripline = workline.strip()

    # Pre-processor lines start with #. Ignore them completely.
    pre_proc = (stripline[0] == "#")

    # Lines that are completely commented start with a bang and are also
    # ignored completely (except if they are actually OpenMP)
    all_comment = (stripline[0] == "!")
    omp_sentinal = all_comment and (stripline[1] == "$")

    # Ignore empty lines or pre-processor directives

    if len(line.strip()) == 0 or pre_proc:
        return line

    # Ignore whole-line comments.
    if all_comment and not omp_sentinal:
        return line

    # First need to make sure that the continuation character and any
    # comments are found correctly. Can't just search for the
    # first ampersand or bang in the line as it is possible that strings
    # (text inside single or double quotes) and comments could contain
    # ampersands or bangs, and these must be ignored when processing the
    # line.
    # The solution is to first find any possibly misleading characters
    # and replace them with temporary substitutes. Then find the location
    # of any continuation character and/or comment in the line, before
    # replacing the substituted characters, and the process the line to
    # correctly align it.
    # The order of substitutions is
    # 1. Find any ampersands that are within pairs of double or single
    #    quotes and replace them with "X".
    #    e.g.
    #    CALL log_info("init_veg", "Doell & Siebert",   &
    #    becomes
    #    CALL log_info("init_veg", "Doell X Siebert",   &
    # 2. Find any bangs that are within pairs of double or single quotes
    #    and replace them with "Y".
    #    e.g.
    #    CALL log_info("init_veg", "Doell method!")  ! Describe method
    #    becomes
    #    CALL log_info("init_veg", "Doell methodY")  ! Describe method
    # 3. Find any ampersands that are within comments and replace them
    #    with "Z".
    #    e.g.
    #    CALL log_info("init_veg", "Doell-Siebert",   & ! Doell & Siebert
    #    becomes
    #    CALL log_info("init_veg", "Doell-Siebert",   & ! Doell Z Siebert
    # Once these replacements have been done, the line should only contain
    # at most one ampersand (as continuation character), and if it contains
    # any bangs, the first one is the start of the comment. Then find
    # the location of these to start any required processing.

    # Find and replace any OpenMP sentinals

    omp_loc = workline.find("!$")

    if omp_loc != -1:
        omp_continue = workline.upper().find("!$OMP&")
        if omp_continue != -1:
            workline = replace_characters(workline, [omp_continue+5], [1],
                                          replchar=" ")
        workline = replace_characters(workline, [omp_loc], [1], replchar="W")

    # Find where there are ampersands or bangs within single or double
    # quotes.

    quoted_amp_locs = find_quoted_char(workline, "&", str_continuation)
    quoted_bang_locs = find_quoted_char(workline, "!", str_continuation)

    # If any ampersands or bangs were found within quotes, replace them
    # with "X" and "Y" respectively.
    if quoted_amp_locs is not None:
        lens = len(quoted_amp_locs) * [1, ]
        workline = replace_characters(workline, quoted_amp_locs, lens,
                                      replchar="X")
    if quoted_bang_locs is not None:
        lens = len(quoted_bang_locs) * [1, ]
        workline = replace_characters(workline, quoted_bang_locs, lens,
                                      replchar="Y")

    # Find where there are ampersands within comments and replace them with
    # "Z" temporarily.
    commented_amp_locs = find_commented_char(workline, "&", str_continuation)

    if commented_amp_locs is not None:
        lens = len(commented_amp_locs) * [1, ]
        workline = replace_characters(workline, commented_amp_locs, lens,
                                      replchar="Z")

    # Check if there is still more than one ampersand in this line and
    # warn if there is.
    if (len(re.findall("&", workline)) > 1):
        amp_loc = workline.find("&")

        # determin if there is a leading ampersand
        beforeline = workline[lp:amp_loc].rstrip()

        if len(beforeline) == 0:
            # the first ampersand is a leading continuation
            workline = replace_characters(workline, [amp_loc], [1],
                                          replchar=" ")
        else:
            raise CharError("&", len(re.findall("&", workline)))

    finder = workline.find

    # Find the first ampersand in the line (if there is one).
    amp_loc = finder("&")

    # Find the first bang in the line to see if it is a comment or OpenMP.
    comment_loc = finder("!")

    # Now the locations of the characters that are needed have been found,
    # replace ampersands and bangs where they were.
    if quoted_amp_locs is not None:
        lens = len(quoted_amp_locs) * [1, ]
        workline = replace_characters(workline, quoted_amp_locs, lens,
                                      replchar="&")
    if quoted_bang_locs is not None:
        lens = len(quoted_bang_locs) * [1, ]
        workline = replace_characters(workline, quoted_bang_locs, lens,
                                      replchar="!")
    if commented_amp_locs is not None:
        lens = len(commented_amp_locs) * [1, ]
        workline = replace_characters(workline, commented_amp_locs, lens,
                                      replchar="&")
    if omp_loc != -1:
        workline = replace_characters(workline, [omp_loc], [1], replchar="!")

    if preclean is True:
        return workline[lp:]

    # If the line contains an unquoted or uncommented ampersand, need
    # to check if it is in the right place.
    if amp_loc != -1:

        # If there is no comment, determin if it is a leading continuation
        # ampersand. If it is remove it; else shift the ampersand (and remove
        # any trailing whitespace).
        if comment_loc == -1:

            # determin if we are a leading ampersand
            beforeline = workline[lp:amp_loc].rstrip()

            if len(beforeline) == 0:
                # the ampersand is a leading continuations
                workline = replace_characters(workline, [amp_loc], [1],
                                              replchar=" ")
            else:
                # Keep the part of the input line before the ampersand (without
                # white space).
                workline = workline[:amp_loc].rstrip()
                temp_length = len(workline)

                # Don't require a space between the end of the code and the
                # continuation character, so maximum length of the code is
                # col - 1.
                if temp_length > col - 1:
                    # If the line is too long, just add the ampersand at the
                    # end (after a space).
                    workline = list(workline)
                    workline.append(" &")
                    workline = "".join(workline)
                else:
                    # Otherwise, add the text to an empty line with an
                    # ampersand at the end.
                    templine = list(" ") * col
                    templine[-1] = "&"
                    workline = list(workline)
                    templine[0:temp_length] = workline[0:temp_length]
                    workline = "".join(templine)

        # If there is a comment after the ampersand then only do anything
        # if the line is too long, in which case can try to take white
        # space to get it down to length.
        elif comment_loc > amp_loc:

            # If the line is too long, first make sure there is only one
            # space between ampersand and comment, and get rid of any
            # trailing whitespace.
            if len(workline.rstrip()) > col:
                comment = workline[comment_loc:].rstrip()
                workline = workline[:amp_loc+1]
                workline = " ".join([workline, comment])

            # If the line is still too long, see if ampersand can be moved
            # left to reduce the length sufficiently. This can only be done
            # if the line doesn't end in a string continuation
            str_cont_test = is_str_continuation(workline, str_continuation)
            check_quote = str_cont_test[SQUOTE] or str_cont_test[DQUOTE]
            str_size_test = len(workline.rstrip()) > col
            if str_size_test and not check_quote:
                # How much do we need to reduce the length to get it short
                # enough?
                nloop = len(workline) - col

                # Get the current location of the ampersand.
                amp_location = amp_loc

                # Cast the line to a list as characters can't be edited in
                # place in a string.
                workline = list(workline)

                # Try cutting down the whitespace one step at a time until
                # there is only one space left.
                for i in range(nloop):

                    if workline[amp_location-1] == " ":
                        # If there is still whitespace that can be removed,
                        # remove it and update the ampersand location.
                        del workline[amp_location-1]
                        amp_location -= 1
                    else:
                        # Ampersand is now next to no-blank text so place
                        # a single space and exit the loop.
                        workline.insert(amp_location, " ")
                        break

                # Join the line back up into a string.
                workline = "".join(workline)

    return workline[lp:]


def check_line_len(line, maxlinelen=DEFAULT_COL):
    '''
    Check line to see if it violates length requirements. If debugging,
    write some information to stdout.
    '''

    return (len(line) > maxlinelen)


def apply_ampersand_shift(lines, col=DEFAULT_COL, fname=None, debug=False,
                          preclean=False):
    '''
    For a lot of lines make sure any continuation ampersands are in the
    same column and return the result
    '''

    not_parsed = []
    output_lines = []
    continuation = False
    pp_continuation = False
    str_continuation = [False, False]
    line_previous = ""

    for iline, line in enumerate(lines):
        try:
            outline = shift_ampersand(line, line_previous, str_continuation,
                                      col, preclean)
        except ParsingError as e:
            if debug:
                print_message("PARSING ERROR",
                              "{0:s} Ampersand shifting has not been "
                              "applied".format(e.msg), iline+1, line=line,
                              fname=fname)
            outline = line
            not_parsed.append(iline)

        # Check for the start of new pre-processor continuation
        if is_pp_continuation(line) and not pp_continuation:
            pp_continuation = True

        # test the line continuation properties of this line
        if pp_continuation:
            if not is_pp_continuation(line):
                pp_continuation = False
                line_previous = ""
        elif continuation:
            if is_continuation(line, str_continuation):
                # check if still string continuation
                str_continuation = is_str_continuation(line, str_continuation)
            else:
                continuation = False
                str_continuation = [False, False]
        else:
            # Finally, detect if the following line is a continuation
            # of this one (and therefore requires no indentation)
            if is_continuation(line, str_continuation):
                continuation = True
                str_continuation = is_str_continuation(line, str_continuation)

        # if we are a pp continuation, save the partial line
        if pp_continuation:
            line_previous = re.sub(r"\\\s*$", "", line_previous) + line

        output_lines.append(outline)

    return output_lines, not_parsed


def apply_check_line_len(lines, fname=None, maxlinelen=DEFAULT_COL,
                         debug=False):
    '''
    For a lot of lines check if any lines are longer than required
    '''

    any_too_long = False
    ilines_too_long = []
    for iline, line in enumerate(lines):
        iline_too_long = check_line_len(line, maxlinelen)
        if iline_too_long:
            any_too_long = True
            ilines_too_long.append(iline)
            if debug:
                print_message("VIOLATION",
                              "Line > {0:d} columns".format(maxlinelen),
                              iline+1, line=line, fname=fname)

    if any_too_long:
        return ilines_too_long
    else:
        return None


def main():
    '''
    Main toplevel function for testing
    '''
    parser = OptionParser(usage="""
    %prog [--column col] [--debug] file_1 [file_2 [file_3] ... ]

    This script will attempt to manipulate white space to make sure ampersands
    are consistently in the same column in each line.

    If the line is still too long, it will minimise its length.

    The optional --column tells which column should be used (default=80)
    """)
    parser.add_option("--column", dest="col", type="int", default=DEFAULT_COL,
                      help="Column in which ampersands should appear")
    parser.add_option("--debug", action="store_true",
                      help="Report useful information")

    (opts, args) = parser.parse_args()

    input_file = args[0]

    if opts.debug:
        print_message("INFO", "Debug mode is on.")

    with open(input_file, "r+") as file_in:
        lines_in = file_in.read().split("\n")
        new_lines, not_parsed = apply_ampersand_shift(lines_in, opts.col,
                                                      fname=input_file,
                                                      debug=opts.debug)
        if opts.debug:
            if len(not_parsed) > 0:
                print_message("WARNING",
                              "Ampersand alignment failed for some lines "
                              "due to parsing errors. Please check lines and "
                              "make sure they are correct.",
                              fname=input_file)
            ilines_too_long = apply_check_line_len(new_lines, input_file,
                                                   maxlinelen=opts.col,
                                                   debug=True)
            if ilines_too_long is not None:
                print_message("WARNING",
                              "Some lines are longer than {0:d} characters. "
                              "Please check and make them "
                              "shorter.".format(opts.col), fname=input_file)

        file_in.seek(0)
        file_in.write("\n".join(new_lines))
        file_in.truncate()


if __name__ == "__main__":
    main()
