## Purpose

This package helps to manage space padding around paired delimiters.

    E.g. {a=>1, b=>2}
          ^
          <SPC>
         { a=>1, b=>2 }
           ^
          <DEL>
         {a=>1, b=>2}

I wrote it to conform to some coding standards, but in some months of use it's
been quite unobtrusive, so it's a global minor-mode now.
