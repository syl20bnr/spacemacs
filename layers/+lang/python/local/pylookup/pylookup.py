#!/usr/bin/env python

"""
Pylookup is to lookup entries from python documentation, especially within
emacs. Pylookup adopts most of ideas from haddoc, lovely toolkit by Martin
Blais.

(usage)
  ./pylookup.py -l ljust
  ./pylookup.py -u http://docs.python.org

"""

from __future__ import with_statement, print_function

import sys
import re
try:
    import cPickle as pickle
except ImportError:
    import pickle

from os.path import join, dirname, exists, abspath, expanduser
from contextlib import closing

try:
    from HTMLParser import HTMLParser
except ImportError:
    from html.parser import HTMLParser

try:
    from urllib2 import urlopen
except ImportError:
    from urllib.request import urlopen

try:
    from urlparse import urlparse
except ImportError:
    from urllib.parse import urlparse

VERBOSE = False
FORMATS = {
             "Emacs" : "{entry}\t({desc})\t[{book}];{url}",
             "Terminal" : "{entry}\t({desc})\t[{book}]\n{url}"
           }

def build_book(s, num):
    """
    Build book identifier from `s`, with `num` links.
    """
    for matcher, replacement in (("library", "lib"),
                                ("c-api", "api"),
                                ("reference", "ref"),
                                ("", "etc")):
        if matcher in s:
            return replacement if num == 1 else "%s/%d" % (replacement, num)

def trim(s):
    """
    Add any globle filtering rules here
    """
    s = s.replace( "Python Enhancement Proposals!", "")
    s = s.replace( "PEP ", "PEP-")
    return s

class Element(object):
    def __init__(self, entry, desc, book, url):
        self.book = book
        self.url = url
        self.desc = desc
        self.entry = entry

    def __format__(self, format_spec):
        return format_spec.format(entry=self.entry, desc=self.desc,
                                  book=self.book, url=self.url)

    def match_insensitive(self, key):
        """
        Match key case insensitive against entry and desc.

        `key` : Lowercase string.
        """
        return key in self.entry.lower() or key in self.desc.lower()

    def match_sensitive(self, key):
        """
        Match key case sensitive against entry and desc.

        `key` : Lowercase string.
        """
        return key in self.entry or key in self.desc

    def match_in_entry_insensitive(self, key):
        """
        Match key case insensitive against entry.

        `key` : Lowercase string.
        """
        return key in self.entry.lower()

    def match_in_entry_sensitive(self, key):
        """
        Match key case sensitive against entry.

        `key` : Lowercase string.
        """
        return key in self.entry


def get_matcher(insensitive=True, desc=True):
    """
    Get `Element.match_*` function.

    >>> get_matcher(0, 0)
    <unbound method Element.match_in_entry_sensitive>
    >>> get_matcher(1, 0)
    <unbound method Element.match_in_entry_insensitive>
    >>> get_matcher(0, 1)
    <unbound method Element.match_sensitive>
    >>> get_matcher(1, 1)
    <unbound method Element.match_insensitive>

    """
    _sensitive = "_insensitive" if insensitive else "_sensitive"
    _in_entry = "" if desc else "_in_entry"
    return getattr(Element, "match{0}{1}".format(_in_entry, _sensitive))


class IndexProcessor( HTMLParser ):
    """
    Extract the index links from a Python HTML documentation index.
    """

    def __init__( self, writer, dirn):
        HTMLParser.__init__( self )

        self.writer     = writer
        self.dirn       = dirn
        self.entry      = ""
        self.desc       = ""
        self.list_entry = False
        self.do_entry   = False
        self.one_entry  = False
        self.num_of_a   = 0
        self.desc_cnt   = 0


    def handle_starttag(self, tag, attr):
        if tag == "dd":
            self.list_entry = True
        elif tag == "dt":
            self.one_entry = True
            self.num_of_a  = 0
        elif tag == "a":
            if self.one_entry:
                self.url = join( self.dirn, dict( attr )[ 'href' ] )


    def handle_endtag(self, tag):
        if tag == "dd":
            self.list_entry = False
        elif tag == "dt":
            self.do_entry = False
        elif tag == "a":
            global VERBOSE
            if self.one_entry:
                if self.num_of_a == 0 :
                    if VERBOSE:
                        self.desc_cnt += 1
                        if self.desc_cnt % 100 == 0:
                            sys.stdout.write("%04d %s\r" \
                                                 % (self.desc_cnt, self.desc.ljust(80)))

                    # extract fist element
                    #  ex) __and__() (in module operator)
                    if not self.list_entry :
                        self.entry = re.sub( "\([^)]+\)", "", self.desc )

                        # clean up PEP
                        self.entry = trim(self.entry)

                        match = re.search( "\([^)]+\)", self.desc )
                        if match :
                            self.desc = match.group(0)

                    self.desc = trim(re.sub( "[()]", "", self.desc ))

                self.num_of_a += 1
                book = build_book(self.url, self.num_of_a)
                e = Element(self.entry, self.desc, book, self.url)

                self.writer(e)


    def handle_data(self, data):
        if self.one_entry and self.num_of_a == 0:
            self.desc = data

def update(db, urls, append=False):
    """Update database with entries from urls.

    `db` : filename to database
    `urls` : list of URL
    `append` : append to db
    """
    mode = "ab" if append else "wb"
    with open(db, mode) as f:
        writer = lambda e: pickle.dump(e, f)
        for url in urls:
            # detech 'file' or 'url' schemes
            parsed = urlparse(url)
            if not parsed.scheme or parsed.scheme == "file":
                dst = abspath(expanduser(parsed.path))
                if not exists(dst):
                    print("Error: %s doesn't exist" % dst)
                    exit(1)
                url = "file://%s" % dst
            else:
                url = parsed.geturl()

            potential_urls = []
            if url.endswith('.html'):
                potential_urls.append(url)
            else:
                # guess index URLs
                # for stdlib, this is genindex-all.html
                # for django, numpy, etc. it's genindex.html
                url = url.rstrip("/")
                potential_urls.append(url + "/genindex-all.html")
                potential_urls.append(url + "/genindex.html")

            success = False
            for index_url in potential_urls:
                try:
                    print("Wait for a few seconds...")
                    print("Fetching index from '%s'" % index_url)

                    index = urlopen(index_url).read()
                    if not issubclass(type(index), str):
                        index = index.decode()

                    parser = IndexProcessor(writer, dirname(index_url))
                    with closing(parser):
                        parser.feed(index)

                    # success, we don't need to try other potential urls
                    print("Loaded index from '%s'" % index_url)
                    success = True
                    break
                except IOError:
                    print("Error: fetching file from '%s'" % index_url)

            if not success:
                print("Failed to load index for input '%s'" % url)


def lookup(db, key, format_spec, out=sys.stdout, insensitive=True, desc=True):
    """Lookup key from database and print to out.

    `db` : filename to database
    `key` : key to lookup
    `out` : file-like to write to
    `insensitive` : lookup key case insensitive
    """
    matcher = get_matcher(insensitive, desc)
    if insensitive:
        key = key.lower()
    with open(db, "rb") as f:
        try:
            while True:
                e = pickle.load(f)
                if matcher(e, key):
                    out.write('%s\n' % format(e, format_spec))
        except EOFError:
            pass

def cache(db, out=sys.stdout):
    """Print unique entries from db to out.

    `db` : filename to database
    `out` : file-like to write to
    """
    with open(db, "rb") as f:
        keys = set()
        try:
            while True:
                e = pickle.load(f)
                k = e.entry
                k = re.sub( "\([^)]*\)", "", k )
                k = re.sub( "\[[^]]*\]", "", k )
                keys.add(k)
        except EOFError:
            pass
        for k in keys:
            out.write('%s\n' % k)

if __name__ == "__main__":
    import optparse
    parser = optparse.OptionParser( __doc__.strip() )
    parser.add_option( "-d", "--db",
                       help="database name",
                       dest="db", default="pylookup.db" )
    parser.add_option( "-l", "--lookup",
                       help="keyword to search",
                       dest="key" )
    parser.add_option( "-u", "--update",
                       help="update url or path",
                       action="append", type="str", dest="url" )
    parser.add_option( "-c", "--cache" ,
                       help="extract keywords, internally used",
                       action="store_true", default=False, dest="cache")
    parser.add_option( "-a", "--append",
                       help="append to the db from multiple sources",
                       action="store_true", default=False, dest="append")
    parser.add_option( "-f", "--format",
                       help="type of output formatting, valid: Emacs, Terminal",
                       choices=["Emacs", "Terminal"],
                       default="Terminal", dest="format")
    parser.add_option( "-i", "--insensitive", default=1, choices=['0', '1'],
                       help="SEARCH OPTION: insensitive search "
                       "(valid: 0, 1; default: %default)")
    parser.add_option( "-s", "--desc", default=1, choices=['0', '1'],
                       help="SEARCH OPTION: include description field "
                       "(valid: 0, 1; default: %default)")
    parser.add_option("-v", "--verbose",
                      help="verbose", action="store_true",
                      dest="verbose", default=False)
    ( opts, args ) = parser.parse_args()

    VERBOSE = opts.verbose
    if opts.url:
        update(opts.db, opts.url, opts.append)
    if opts.cache:
        cache(opts.db)
    if opts.key:
        lookup(opts.db, opts.key, FORMATS[opts.format],
               insensitive=int(opts.insensitive), desc=int(opts.desc))
