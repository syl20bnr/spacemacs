# global_conf.py --- ycmd global configuration file for Spacemacs
#
# Copyright (c) 2012-2020 Sylvain Benner & Contributors
#
# Author: Thomas de Beauchene <thomas.de.beauchene@gmail.com>
# URL: https://github.com/syl20bnr/spacemacs
#
# This file is not part of GNU Emacs.
#
# License: GPLv3
#
# This script tries to get the compilation flags for a translation unit using
# the following logic:
#
# 1) If there is a compile_commands.json in a parent directory:
#   a) If the file is a header file:
#     - search for the header file itself in db
#     - search for a sibling source file in the same directory (i.e. a source
#       file with the same name but different extension)
#     - search for a source file that includes our header's path
#     - search for the nearest source file in db
#
#   b) If the file is a source file:
#     - search for the source file itself
#     - search for the nearest source file in db
#
# 2) If no compile_commands.json, search for a .clang_complete:
#   - get flags from .clang_complete
#
# 3) Always try to add extra flags from a .ycm_extra_flags file in a parent
#    directory. (like --sysroot="/path/to/your/toolchain/libc" if you are cross-compiling)
#
# Thanks to Jonas Devlieghere and Gabor Marton for their work on which this code is based.
# https://jonasdevlieghere.com/a-better-youcompleteme-config/
# https://github.com/martong/ycm_extra_conf.jsondb

import itertools
import json
import logging
import os
import os.path
import re
import ycm_core

# From pyhon 3 on, zip is already a generator and there is no izip in
# itertools.
try:
    from itertools import izip as zip
except ImportError:
    pass

SOURCE_EXTENSIONS = ['.cpp', '.cxx', '.cc', '.c', '.m', '.mm',
                     '.CPP', '.CXX', '.CC', '.C', '.M', '.MM']
HEADER_EXTENSIONS = ['.h', '.hxx', '.hpp', '.hh'
                     '.H', '.HXX', '.HPP', '.HH']
SUBDIRS = ['build']

# This function is called by ycmd.
def FlagsForFile(filename):
    logging.info("%s: Getting flags for %s" % (__file__, filename))
    root = os.path.realpath(filename)
    flags = FlagsFromCompilationDatabase(root, filename)
    if not flags:
        flags = FlagsFromClangComplete(root, filename)
    extra_flags = GetUserExtraFlags(filename)
    if extra_flags:
        if flags:
            flags += extra_flags
        else:
            flags = extra_flags
    if flags:
        flags = [ flag for flag in flags if not flag.startswith("-m") ] # strip -m flags
        logging.info("%s: Flags = [\n\t\t%s\n]"
                     % (os.path.basename(filename), "\n\t\t".join(flags)))
    else:
        flags = []
        logging.error("%s: No flags were found !" % (os.path.basename(filename)))
    return { 'flags': flags, 'do_cache': True }

def FlagsFromClangComplete(root, filename):
    try:
        clang_complete_path = FindNearest(root, '.clang_complete', filename)
        clang_complete_flags = open(clang_complete_path, 'r').read().splitlines()
        logging.info("%s: Using %s" % (os.path.basename(filename), clang_complete_path))
        return MakeRelativePathsInFlagsAbsolute(clang_complete_flags,
                                                os.path.dirname(clang_complete_path))
    except:
        return None

def FlagsFromCompilationDatabase(root, filename):
    try:
        database_path = FindNearest(root, 'compile_commands.json', filename, subdirs=SUBDIRS)
        database = ycm_core.CompilationDatabase(os.path.dirname(database_path))
        if not database:
            logging.info("%s: Compilation database file found but unable to load"
                         % os.path.basename(filename))
            return None
        extension = os.path.splitext(filename)[1]
        if extension in HEADER_EXTENSIONS:
            flags = GetFlagsForHeader(database_path, database, filename)
        else:
            flags = GetFlagsForSourceFile(database_path, database, filename)
        if not flags:
            logging.info("%s: No compilation info for %s in compilation database"
                         % (os.path.basename(filename), filename))
            return None
        return MakeRelativePathsInFlagsAbsolute(flags.compiler_flags_,
                                                flags.compiler_working_dir_)
    except Exception as e:
        logging.info("%s: Could not get compilation flags from db: %s"
                     % (os.path.basename(filename), e))
        return None

def GetFlagsForHeader(database_path, database, filename):
    flags = FindFileInDb(database, filename)
    if flags:
        return flags
    flags = FindSiblingFileForHeader(database, filename)
    if flags:
        return flags
    flags = SearchForTranslationUnitWhichIncludesPath(database_path,
                                                      database,
                                                      os.path.dirname(filename),
                                                      filename)
    if flags:
        return flags
    return FindNearestSourceFileInDb(database_path, database, filename)

def GetFlagsForSourceFile(database_path, database, filename):
    flags = FindFileInDb(database, filename)
    if flags:
        return flags
    return FindNearestSourceFileInDb(database_path, database, filename)

def FindNearest(path, target, filename, subdirs=[]):
    candidates = [os.path.join(path, target)]
    for subdir in subdirs:
        candidates.append(os.path.join(path, subdir, target))
    for candidate in candidates:
        if(os.path.isfile(candidate) or os.path.isdir(candidate)):
            logging.info("%s: Found nearest %s at %s"
                         % (os.path.basename(filename), target, candidate))
            return candidate
    parent = os.path.dirname(os.path.abspath(path))
    if(parent == path):
        raise RuntimeError("could not find %s" % target)
    return FindNearest(parent, target, filename, subdirs)

def FindFileInDb(database, filename):
    logging.info("%s: Trying to find file in database..."
                 % (os.path.basename(filename)))
    flags = database.GetCompilationInfoForFile(filename)
    if flags.compiler_flags_:
        logging.info("%s: Found file in database."
                     % (os.path.basename(filename)))
        return flags
    logging.info("%s: File not found in compilation db."
                 % (os.path.basename(filename)))
    return None

def FindSiblingFileForHeader(database, filename):
    logging.info("%s: Trying to find a sibling source file for that header in database..."
                 % (os.path.basename(filename)))
    basename = os.path.splitext(filename)[0]
    for extension in SOURCE_EXTENSIONS:
        replacement_file = basename + extension
        if os.path.exists(replacement_file):
            flags = database.GetCompilationInfoForFile(replacement_file)
            if flags.compiler_flags_:
                logging.info("%s: Found sibling source file: %s"
                             % (os.path.basename(filename), replacement_file))
                return flags
    logging.info("%s: Did not find sibling source file."
                 % (os.path.basename(filename)))
    return None

def FindNearestSourceFileInDb(database_path, database, srcfile):
    logging.info("%s: Trying to find nearest source file in database..."
                 % (srcfile))
    filename, flags = DoFindNearestSourceFileInDb(database_path, database, srcfile, None)
    if flags:
        logging.info("%s: Found nearest source file from %s: %s"
                     % (os.path.basename(srcfile), srcfile, filename))
        return flags
    logging.info("%s: Could not find nearest source file from %s in compilation db."
                % (srcfile, srcfile))
    return None

# Search subdirectories recursively, then do the same recursively for parent
# directories until a file was found or we have searched the database's directory
def DoFindNearestSourceFileInDb(database_path, database, directory, skip):
    for root, dirnames, filenames in os.walk(directory):
        if os.path.basename(skip) in dirnames:
            dirnames.remove(os.path.basename(skip))
        for filename in filenames:
            if filename.endswith(tuple(SOURCE_EXTENSIONS)):
                flags = database.GetCompilationInfoForFile(os.path.join(root, filename))
                if flags.compiler_flags_:
                    return os.path.join(root, filename), flags
    if database_path == directory or os.path.dirname(directory) == directory:
        return None, None
    return DoFindNearestSourceFileInDb(database_path, database, os.path.dirname(directory), directory)

def Pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = itertools.tee(iterable)
    next(b, None)
    return zip(a, b)

def RemoveClosingSlash(path):
    if path.endswith('/'):
        path = path[:-1]
    return path

def SearchForTranslationUnitWhichIncludesPath(database_path, database, path, filename):
    logging.info("%s: Trying to find a translation unit that includes our header's path..."
                 % (os.path.basename(filename)))
    with open(database_path, 'r') as f:
        jsonDb = json.load(f)
    path = RemoveClosingSlash(os.path.abspath(path))
    found = []
    for translationUnit in jsonDb:
        buildDir = translationUnit["directory"]
        switches = translationUnit["command"].split()
        for currentSwitch, nextSwitch in Pairwise(switches):
            matchObj = re.match(r'(-I|-isystem)(.*)', currentSwitch)
            includeDir = ""
            isIncFlag = False
            if currentSwitch == "-I" or currentSwitch == "-isystem":
                includeDir = nextSwitch
                isIncFlag = True
            elif matchObj:
                includeDir = matchObj.group(2)
                isIncFlag = True
            if not isIncFlag:
                continue
            includeDir = RemoveClosingSlash(os.path.abspath(os.path.join(buildDir, includeDir)))
            # Check all the parent dirs in path
            pathCopy = path
            distance = 0
            while pathCopy != os.path.abspath(os.sep):
                if includeDir == pathCopy:
                    found.append((distance, str(translationUnit["file"])))
                distance += 1
                pathCopy, tail = os.path.split(pathCopy)
    found.sort()
    if len(found) == 0:
        logging.info("%s: Did not find translation unit which includes path %s"
                     % (os.path.basename(filename), path))
        return None
    else:
        result = found[0][1]
        logging.info("%s: Found best source file which includes path: %s"
                     % (os.path.basename(filename), result))
        return database.GetCompilationInfoForFile(result)

def GetUserExtraFlags(filename):
    try:
        extra_flags_file = FindNearest(os.path.dirname(filename), ".ycm_extra_flags", filename)
    except:
        logging.info("%s: No extra flags."
                 % (os.path.basename(filename)))
        return None
    with open(extra_flags_file, 'r') as f:
        lines = f.readlines()
    lines = [ line[0:line.find("#")].split() for line in lines ]
    lines = list(itertools.chain.from_iterable(lines))
    logging.info("%s: Extra flags = [\n\t\t%s\n]"
                 % (os.path.basename(filename), "\n\t\t".join(lines)))
    return lines

def MakeRelativePathsInFlagsAbsolute(flags, working_directory):
    if not working_directory:
        return list(flags)
    new_flags = []
    make_next_absolute = False
    for flag in flags:
        new_flag = flag
        if make_next_absolute:
            make_next_absolute = False
            if not flag.startswith('/'):
                new_flag = os.path.join(working_directory, flag)
        for path_flag in [ '-isystem', '-I', '-iquote', '--sysroot=' ]:
            if flag == path_flag:
                make_next_absolute = True
                break
            if flag.startswith(path_flag):
                path = flag[ len(path_flag): ]
                new_flag = path_flag + os.path.join(working_directory, path)
                break
        if new_flag:
            new_flags.append(new_flag)
    return new_flags
