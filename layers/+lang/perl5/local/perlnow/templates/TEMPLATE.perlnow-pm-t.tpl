# Perl test file, can be run like so:
#   `perl (>>>FILE<<<)'
#         (>>>AUTHOR<<<)     (>>>VC_DATE<<<)

use warnings;
use strict;
$|=1;
my $DEBUG = 1;              # TODO set to 0 before ship
use Data::Dumper;

use Test::More;
BEGIN { plan tests => 3 }; # TODO revise test count


use_ok( '(>>>PERL_MODULE_NAME<<<)' );


ok(1, "Traditional: If we made it this far, we're ok.");

# Insert your test code below.  Consult perldoc Test::More for help.

(>>>POINT<<<)
